/*  Title:      AI/src/Tools/get_session_state_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

import scala.collection.immutable.VectorMap

object Get_Session_State_Tool {
  def delay(options: Options): Time = options.seconds("pide_ai_get_session_state_delay")
  def wait_ratio(options: Options): Double =
    options.real("pide_ai_get_session_state_wait_ratio")
  val progress_delay_option: String = "pide_ai_get_session_state_progress_delay"
  def progress_delay(options: Options): Time = options.seconds(progress_delay_option)

  object Request {
    val show_cmds_running_arg = JSON_Schema.Arg.default(
      "show_cmds_processing", "", JSON_Schema.Format.bool, true)
    val show_terminated_slow_arg = JSON_Schema.Arg.default(
      "show_terminated_slow", s"Above ${PIDE_AI_Tool_Schema.long_running_arg.name}.",
      JSON_Schema.Format.bool, false)
    val show_bad_arg = JSON_Schema.Arg.default(
      "show_bad", "Show bad markup positions.",
      JSON_Schema.Format.bool, false)
    val show_dynamic_thys_arg = PIDE_AI_Tool_Schema.show_thy_statuses_arg(
      "show_dynamic_thys", PIDE_AI_Theory_Status.Statuses.Show.not_ok)
    val show_base_session_thys_arg = JSON_Schema.Arg.default(
      "show_base_session_thys", "", JSON_Schema.Format.bool, false)
    val show_session_dirs_arg = JSON_Schema.Arg.default(
      "show_session_dirs", "Directories from which a session resolves files.",
      JSON_Schema.Format.bool, false)
    val show_options_arg = JSON_Schema.Arg.default(
      "show_options",
      "Isabelle session options. " +
      s"${quote("changed")} shows options that differ from their default.",
      JSON_Schema.Format.enumeration(PIDE_AI_Session_State.Show.Options.values),
      PIDE_AI_Session_State.Show.Options.none)
    val show_statistics_arg = JSON_Schema.Arg.default(
      "show_statistics",
      "Each session's ML and the server's JVM statistics (heap, threads,...).",
      JSON_Schema.Format.bool, false)
    val timeout_arg = JSON_Schema.Arg.default("timeout_ms", "Max. wait.",
      PIDE_AI_Tool_Schema.ms_format(0), Time.zero)
    val progress_delay_arg = PIDE_AI_Tool_Schema.progress_delay_arg(progress_delay_option)

    val args: List[JSON_Schema.Arg[?]] =
      List(show_cmds_running_arg, show_terminated_slow_arg, PIDE_AI_Tool_Schema.long_running_arg,
        show_bad_arg, show_dynamic_thys_arg, show_base_session_thys_arg, show_session_dirs_arg,
        show_options_arg, show_statistics_arg, timeout_arg, PIDE_AI_Tool_Schema.stop_on_arg,
        progress_delay_arg)

    def from_json(args: JSON.Object.T): Request =
      Request(
        PIDE_AI_Session_State.Show(
          cmds_running = show_cmds_running_arg.get(args),
          terminated_slow = show_terminated_slow_arg.get(args),
          bad = show_bad_arg.get(args),
          dynamic_thys = show_dynamic_thys_arg.get(args),
          base_session_thys = show_base_session_thys_arg.get(args),
          session_dirs = show_session_dirs_arg.get(args),
          options = show_options_arg.get(args),
          statistics = show_statistics_arg.get(args)),
        PIDE_AI_Tool_Schema.long_running_arg.get(args), timeout_arg.get(args),
        PIDE_AI_Tool_Schema.stop_on_arg.get(args), progress_delay_arg.get(args))
  }

  sealed case class Request(
    show: PIDE_AI_Session_State.Show,
    long_running_threshold: Option[Time],
    timeout: Time,
    stop_on: PIDE_AI_Tool_Util.Stop_On,
    progress_delay: Option[Time])

  def state(
    selections: List[PIDE_AI_Session_State.Selection],
    request: Request,
    progress: PIDE_AI_Progress,
    cwd: Option[Path]
  ): JSON.Object.T = {
    def sessions_json(
      initial_thys: List[List[Document.Node.Name]]
    )(states: List[PIDE_AI_Session_State]): JSON.Object.T =
      JSON_Object("sessions" -> initial_thys.zip(states).map((thys, state) =>
        state.json(thys, request.show,
          request.long_running_threshold.getOrElse(state.session.long_running_threshold), cwd)))
    val (initial_thys, states, timed_out) = selections.headOption match {
      case Some(selection) if !request.timeout.is_zero =>
        val initial_thys = selections.map(PIDE_AI_Session_State(_).dynamic_thys)
        val options = selection.session.session_options
        val (states, timed_out) = PIDE_AI_Session_State.await(selections,
          states => request.stop_on.stop_thys(states.flatMap(_.statuses)), Some(request.timeout),
          delay(options), wait_ratio(options), sessions_json(initial_thys),
          request.progress_delay.getOrElse(progress_delay(options)), progress)
        (initial_thys, states, timed_out)
      case _ =>
        val states = selections.map(PIDE_AI_Session_State(_))
        (states.map(_.dynamic_thys), states, false)
    }
    JSON.optional("timed_out" -> Option.when(timed_out)(true)) ++
    sessions_json(initial_thys)(states) ++
    JSON_Object.if_proper(request.show.statistics, PIDE_AI_Session_State.jvm_stats_json())
  }
}

class Get_Session_State_Tool extends PIDE_AI_Tool("get_session_state")
with PIDE_AI_Tool.Read_Only {
  def description: String =
    "State of running sessions, information for their theories " +
    "(status, commands being processed,...), resource statistics,..."

  val filter_thys_arg = JSON_Schema.Arg.default(
    "filter_thys",
    "Session id to theory nodes whose information is returned. " +
    "Default for entry-less session: all dynamic theories.",
    JSON_Schema.Format.dictionary(
      JSON_Schema.Format.list(JSON_Schema.Format.string)),
    VectorMap.empty)

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(PIDE_AI_Tool_Schema.running_sessions_arg, filter_thys_arg) :::
    Get_Session_State_Tool.Request.args)

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: PIDE_AI_Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val selected_sessions = PIDE_AI_Tool_Util.running_sessions_param(server.sessions, args)
      val filter_thys = filter_thys_arg.get(args)
      val selected_ids = selected_sessions.map(_.id)
      val unknown = filter_thys.keysIterator.filterNot(selected_ids.toSet).toList
      if (unknown.nonEmpty) {
        error(s"Session(s) ${commas_quote(unknown)} of ${filter_thys_arg.name} not contained in " +
          s"selection ${commas_quote(selected_ids)}.")
      }
      val selections = selected_sessions.map(session =>
        PIDE_AI_Session_State.Selection(session, filter_thys.get(session.id).map { names =>
          val resources = session.resources
          val nodes = session.snapshot().version.nodes
          val dynamic_thys = PIDE_AI_Nodes.Loaded(resources, nodes).dynamic.toSet
          val (thys, others) =
            names.map(resources.node_name(nodes, _, context.cwd)).partition(dynamic_thys)
          if (others.nonEmpty) error("Not a dynamic theory in session " + quote(session.id) +
            ": " + commas_quote(others.map(resources.print_node_name(_, context.cwd))))
          thys
        }))
      Get_Session_State_Tool.state(selections, Get_Session_State_Tool.Request.from_json(args),
        progress, context.cwd)
    }
}

class Get_Session_State_Tools extends PIDE_AI_Tools(new Get_Session_State_Tool)
