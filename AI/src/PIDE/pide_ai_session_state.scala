/*  Title:      AI/src/PIDE/pide_ai_session_state.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Session_State {
  object Show {
    enum Dynamic_Thys { case none, names, summary }
    enum Options { case none, changed, all }
  }

  sealed case class Show(
    cmds_running: Boolean,
    dynamic_thys: Show.Dynamic_Thys,
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

  def json(
    session: PIDE_AI_Session,
    origins: Option[List[String]],
    show: Show,
    long_running_threshold: Time,
    cwd: Option[Path]
  ): JSON.Object.T = {
    lazy val snapshot = session.snapshot()
    lazy val theories = PIDE_AI_Nodes.Loaded(session.resources, snapshot.version.nodes)
    lazy val dynamic_thys = origins match {
      case None => theories.dynamic
      case Some(origins) =>
        val selected = origins.map(session.node_name(_, cwd)).toSet
        theories.dynamic.filter(selected.contains)
    }
    def print_origins(names: List[Document.Node.Name]): List[String] =
      names.map(session.print_origin(_, cwd))
    JSON_Object("session_id" -> session.id, "base_session" -> session.base_session) ++
    JSON.optional(
      PIDE_AI_Document_Command.States.cmds_key(PIDE_AI_Command_Status.running.name) ->
      Option.when(show.cmds_running) {
        PIDE_AI_Document_Command.nodes_running_json(session.resources,
          PIDE_AI_Document_Command.Selection.nodes(session, snapshot, dynamic_thys),
          1, long_running_threshold, cwd)
      }.filter(_.nonEmpty)) ++
    (show.dynamic_thys match {
      case Show.Dynamic_Thys.none => JSON.Object.empty
      case Show.Dynamic_Thys.names => JSON_Object("thys" -> print_origins(dynamic_thys))
      case Show.Dynamic_Thys.summary =>
        PIDE_AI_Theory.Statuses.json(session.resources,
          PIDE_AI_Theory.Statuses(snapshot, dynamic_thys), cwd)
    }) ++
    JSON.optional("base_session_thys" ->
      Option.when(show.base_session_thys)(print_origins(theories.base_session))) ++
    JSON.optional("session_dirs" -> Option.when(show.session_dirs)(
      session.directories().map(PIDE_AI_Util.print_path(_, cwd)))) ++
    options_json(show.options, session.options) ++
    JSON_Object.if_proper(show.statistics, {
      val statistics = session.runtime_statistics()
      if (statistics.isEmpty) JSON_Object("note" -> "no ML statistics reported yet")
      else ml_stats_json(statistics)
    })
  }
}
