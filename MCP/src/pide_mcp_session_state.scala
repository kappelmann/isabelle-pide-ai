/*  Title:      MCP/src/pide_mcp_session_state.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Session_State {
  object Include {
    enum Options { case `false`, `true`, changed }

    object Options {
      private val strings = JSON_Schema.Format.enumeration(values)
      val format: JSON_Schema.Format[Options] = JSON_Schema.Format(
        JSON_Object("type" -> List("string", "boolean"),
          "enum" -> (List[JSON.T](false, true) ::: values.map(strings(_)).toList)),
        {
          case JSON.Value.Boolean(mode) => Some(if (mode) `true` else `false`)
          case json => strings.unapply(json)
        },
        {
          case `false` => false
          case `true` => true
          case mode => strings(mode)
        })
    }
  }

  sealed case class Include(
    cmds_processing: Boolean,
    thy_status: Boolean,
    loaded_thys: Boolean,
    session_dirs: Boolean,
    options: Include.Options,
    statistics: Boolean)

  def options_json(mode: Include.Options, options: Options): JSON.Object.T = {
    def json(entries: Iterator[Options.Entry]): JSON.Object.T =
      JSON_Object("options" -> JSON_Object(entries.map(entry => entry.name -> entry.value)))
    mode match {
      case Include.Options.`false` => JSON.Object.empty
      case Include.Options.`true` => json(options.iterator)
      case Include.Options.changed =>
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

  private def space_entry(key: String, bytes: Option[Double]): Option[(String, JSON.T)] =
    bytes.map(bytes => key -> Space.B(bytes).print)

  private def space_average_maximum(
    ml: ML_Statistics,
    key: String,
    field: ML_Statistics.Field
  ): List[Option[(String, JSON.T)]] = {
    def entry(suffix: String, aggregate: ML_Statistics => Double) =
      Option.when(ml.content.nonEmpty)(key + suffix -> Space.MiB(aggregate(ml)).print)
    List(entry("_average", _.average(field)), entry("_maximum", _.maximum(field)))
  }

  private def count_entry(
    props: Properties.T,
    key: String,
    field: ML_Statistics.Field
  ): Option[(String, JSON.T)] =
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
    session: PIDE_MCP_Session,
    origins: Option[List[String]],
    include: Include
  ): JSON.Object.T = {
    lazy val snapshot = session.snapshot()
    lazy val theories = PIDE_MCP_Theory.Loaded(session, snapshot.version.nodes)
    lazy val names = origins match {
      case None => theories.dynamic
      case Some(origins) =>
        val selected = origins.map(origin => session.origin(session.node_name(origin))).toSet
        theories.dynamic.filter(name => selected.contains(session.origin(name)))
    }
    JSON_Object("session" -> session.id, "base_session" -> session.base_session) ++
    JSON.optional(PIDE_MCP_Command.Status.key(PIDE_MCP_Command.Status.running.name) ->
      Option.when(include.cmds_processing) {
        PIDE_MCP_Theory.commands_running_json(session, snapshot, names)
      }.filter(_.nonEmpty)) ++
    JSON_Object.if_proper(include.thy_status,
      PIDE_MCP_Theory.Status.statuses_json(
        PIDE_MCP_Theory.Status.statuses(session, snapshot, names))) ++
    JSON.optional("loaded_thys" -> Option.when(include.loaded_thys)(theories.json)) ++
    JSON.optional("session_dirs" ->
      Option.when(include.session_dirs)(session.directories().map(_.implode))) ++
    options_json(include.options, session.options) ++
    JSON_Object.if_proper(include.statistics, {
      val statistics = session.runtime_statistics()
      if (statistics.isEmpty) JSON_Object("note" -> "no ML statistics reported yet")
      else ml_stats_json(statistics)
    })
  }
}
