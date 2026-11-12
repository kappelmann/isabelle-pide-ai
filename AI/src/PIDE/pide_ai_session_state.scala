/*  Title:      AI/src/PIDE/pide_ai_session_state.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Session_State {
  object Show {
    enum Options { case none, changed, all }
  }

  sealed case class Show(
    cmds_running: Boolean,
    terminated_slow: Boolean,
    bad: Boolean,
    dynamic_thys: PIDE_AI_Theory_Status.Statuses.Show,
    base_session_thys: Boolean,
    session_dirs: Boolean,
    options: Show.Options,
    statistics: Boolean)

  def options_json(mode: Show.Options, options: Options): JSON.Object.T = {
    def json(entries: Iterator[Options.Entry]): JSON.Object.T =
      JSON_Object("options" -> JSON_Object(entries.map(entry => entry.name -> entry.value)))
    mode match {
      case Show.Options.none => JSON.Object.empty
      case Show.Options.all => json(options.iterator)
      case Show.Options.changed =>
        json(options.iterator.filter(entry => entry.value != entry.default_value))
    }
  }

  val ML_Heap_Used = new ML_Statistics.Field_MiB("size_heap_used", description = "heap used") {
    override val domain: List[String] = List(ML_Statistics.Heap_Size.name,
      ML_Statistics.Heap_Free_Minor.name)
    override def unapply(props: Properties.T): Option[Double] =
      for {
        size <- ML_Statistics.Heap_Size.unapply(props)
        free <- ML_Statistics.Heap_Free_Minor.unapply(props)
      } yield Space.B(size).used(Space.B(free)).B
  }

  def ml_heap_collected(props: Properties.T): Boolean =
    List(ML_Statistics.GCs_Minor, ML_Statistics.GCs_Major).exists(_.unapply(props).exists(_ > 0))

  private def space_entry(key: String, bytes: Option[Double]): Option[JSON.Object.Entry] =
    bytes.map(bytes => key -> Space.B(bytes).print)

  private def space_average_maximum(
    ml: ML_Statistics,
    key: String,
    field: ML_Statistics.Field
  ): List[Option[JSON.Object.Entry]] = {
    def entry(suffix: String, aggregate: ML_Statistics => Double) =
      Option.when(ml.content.nonEmpty)(key + suffix -> Space.MiB(aggregate(ml)).print)
    List(entry("_average", _.average(field)), entry("_maximum", _.maximum(field)))
  }

  private def count_entry(
    props: Properties.T,
    key: String,
    field: ML_Statistics.Field
  ): Option[JSON.Object.Entry] =
    field.unapply(props).map(count => key -> count.toLong)

  def ml_stats_json(statistics: List[Properties.T]): JSON.Object.T = {
    val ml = ML_Statistics(statistics)
    val after_gc = ML_Statistics(statistics.filter(ml_heap_collected))
    val latest = statistics.last
    JSON_Object.flatten(List(
      Some("samples" -> statistics.length.toLong),
      Some("duration_ms" -> Time.seconds(ml.duration).ms)) :::
      space_average_maximum(after_gc, "ml_heap_used_after_gc", ML_Heap_Used) :::
      space_average_maximum(ml, "ml_heap", ML_Statistics.Heap_Size) :::
      space_average_maximum(ml, "ml_code", ML_Statistics.Program_Code) :::
      space_average_maximum(ml, "ml_stack", ML_Statistics.Program_Stack) :::
      List(count_entry(latest, "tasks_running", ML_Statistics.Tasks_Running),
        count_entry(latest, "tasks_total", ML_Statistics.Tasks_Total),
        count_entry(latest, "workers_active", ML_Statistics.Workers_Active),
        count_entry(latest, "workers_total", ML_Statistics.Workers_Total),
        count_entry(latest, "ml_threads", ML_Statistics.Threads_ML)))
  }

  def java_heap_collected(props: Properties.T): Boolean =
    ML_Statistics.Java_Heap_Size_Major.unapply(props).exists(_ > 0)

  def java_heap(props: Properties.T, field: ML_Statistics.Field): Option[Double] =
    if (java_heap_collected(props)) field.unapply(props) else None

  def jvm_stats_json(): JSON.Object.T = {
    val props = ML_Statistics.jvm_statistics()
    JSON_Object.flatten(
      space_entry("java_heap_used_after_gc", java_heap(props, ML_Statistics.Java_Heap_Used_Major)),
      space_entry("java_heap", java_heap(props, ML_Statistics.Java_Heap_Size_Major)),
      count_entry(props, "java_workers_active", ML_Statistics.Java_Workers_Active),
      count_entry(props, "java_workers_total", ML_Statistics.Java_Workers_Total),
      count_entry(props, "java_threads_total", ML_Statistics.Java_Threads_Total))
  }

  sealed case class Selection(
    session: PIDE_AI_Session,
    thys: Option[List[Document.Node.Name]])

  def apply(selection: Selection): PIDE_AI_Session_State = {
    val session = selection.session
    val view = session.node_cache.view(session.snapshot())
    val loaded = PIDE_AI_Nodes.Loaded(session.resources, view.snapshot.version.nodes)
    PIDE_AI_Session_State(session, view, loaded.base_session,
      selection.thys.fold(loaded.dynamic)(thys => loaded.dynamic.filter(thys.toSet)))
  }

  def await(
    selections: List[Selection],
    stop: List[PIDE_AI_Session_State] => Boolean,
    timeout: Option[Time],
    delay: Time,
    wait_ratio: Double,
    message: List[PIDE_AI_Session_State] => JSON.T,
    progress_delay: Time,
    progress: PIDE_AI_Progress
  ): (List[PIDE_AI_Session_State], Boolean) =
    PIDE_AI_Progress.await_adaptive(progress, message, timeout, delay, wait_ratio,
      progress_delay) {
      selections.foreach(_.session.require_ready())
      val states = selections.map(PIDE_AI_Session_State(_))
      (states, stop(states))
    }
}

sealed case class PIDE_AI_Session_State(
  session: PIDE_AI_Session,
  view: PIDE_AI_Node_Cache.View,
  base_session_thys: List[Document.Node.Name],
  dynamic_thys: List[Document.Node.Name]
) {
  lazy val statuses: List[PIDE_AI_Theory_Status] =
    PIDE_AI_Theory_Status.Statuses(view, dynamic_thys)

  def json(
    initial_thys: List[Document.Node.Name],
    show: PIDE_AI_Session_State.Show,
    long_running_threshold: Time,
    cwd: Option[Path]
  ): JSON.Object.T = {
    def print_nodes(names: List[Document.Node.Name]): List[String] =
      names.map(session.resources.print_node_name(_, cwd))
    def optional_entry(key: String, shown: Boolean)(json: => JSON.Object.T): JSON.Object.T =
      JSON.optional(key -> Option.when(shown)(json).filter(_.nonEmpty))
    JSON_Object("session_id" -> session.id, "base_session" -> session.base_session) ++
    optional_entry(PIDE_AI_Document_Command.States.cmds_key(PIDE_AI_Command_Status.running.name),
      show.cmds_running)(PIDE_AI_Document_Command.nodes_running_json(session.resources,
        PIDE_AI_Document_Command.Selection.running(session, view, dynamic_thys), 1,
        long_running_threshold, cwd)) ++
    optional_entry(PIDE_AI_Document_Command.States.cmds_key("terminated_slow"),
      show.terminated_slow)(PIDE_AI_Document_Command.nodes_terminated_slow_json(
        session.resources, PIDE_AI_Document_Command.Selection.terminated_slow(session, view,
          dynamic_thys, long_running_threshold), 1, long_running_threshold, cwd)) ++
    optional_entry(Markup.BAD, show.bad)(
      PIDE_AI_Document_Command.nodes_bad_json(session.resources, view,
        dynamic_thys.flatMap(thy =>
          thy :: PIDE_AI_Nodes.blobs_names(view.snapshot.version.nodes(thy))).distinct, cwd)) ++
    JSON.optional("thys_loaded" ->
      proper_list(print_nodes(dynamic_thys.filterNot(initial_thys.toSet)))) ++
    JSON.optional("thys_unloaded" ->
      proper_list(print_nodes(initial_thys.filterNot(dynamic_thys.toSet)))) ++
    PIDE_AI_Theory_Status.Statuses.json(session.resources, statuses, show.dynamic_thys, cwd) ++
    JSON.optional("base_session_thys" ->
      Option.when(show.base_session_thys)(print_nodes(base_session_thys))) ++
    JSON.optional("session_dirs" -> Option.when(show.session_dirs)(
      session.directories().map(PIDE_AI_Util.print_path(_, cwd)))) ++
    PIDE_AI_Session_State.options_json(show.options, session.session_options) ++
    JSON_Object.if_proper(show.statistics, {
      val statistics = session.recent_runtime_statistics()
      if (statistics.isEmpty) JSON_Object("note" -> "no ML statistics reported yet")
      else PIDE_AI_Session_State.ml_stats_json(statistics)
    })
  }
}
