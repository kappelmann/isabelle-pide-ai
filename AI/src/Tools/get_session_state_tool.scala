/*  Title:      AI/src/Tools/get_session_state_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

import scala.collection.immutable.VectorMap

class Get_Session_State_Tool extends PIDE_AI_Tool("get_session_state")
with PIDE_AI_Tool.Read_Only {
  def description: String =
    "State of running sessions, information for their theories " +
    "(status, commands being processed,...), resource statistics,..."

  val filter_nodes_arg = JSON_Schema.Arg.default(
    "filter_nodes",
    "Session id to theory nodes whose information is returned. " +
    "Default for entry-less session: all dynamic theories.",
    JSON_Schema.Format.dictionary(
      JSON_Schema.Format.list(JSON_Schema.Format.string)),
    VectorMap.empty)
  val show_cmds_running_arg = JSON_Schema.Arg.default(
    "show_cmds_processing", "", JSON_Schema.Format.bool, true)
  val show_terminated_slow_arg = JSON_Schema.Arg.default(
    "show_terminated_slow", s"Above ${PIDE_AI_Tool_Schema.long_running_arg.name}.",
    JSON_Schema.Format.bool, false)
  val show_bad_arg = JSON_Schema.Arg.default(
    "show_bad", "Show bad markup positions. Slow for large theories.",
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

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(PIDE_AI_Tool_Schema.running_sessions_arg, filter_nodes_arg,
      show_cmds_running_arg, show_terminated_slow_arg, PIDE_AI_Tool_Schema.long_running_arg,
      show_bad_arg, show_dynamic_thys_arg, show_base_session_thys_arg,
      show_session_dirs_arg, show_options_arg, show_statistics_arg))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: PIDE_AI_Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val selected_sessions = PIDE_AI_Tool_Util.running_sessions_param(server.sessions, args)
      val filter_nodes = filter_nodes_arg.get(args)
      val selected_ids = selected_sessions.map(_.id)
      val unknown = filter_nodes.keysIterator.filterNot(selected_ids.toSet).toList
      if (unknown.nonEmpty) {
        error(s"Session(s) ${commas_quote(unknown)} of filter_nodes not contained in " +
          s"selection ${commas_quote(selected_ids)}.")
      }
      val show = PIDE_AI_Session_State.Show(
        cmds_running = show_cmds_running_arg.get(args),
        terminated_slow = show_terminated_slow_arg.get(args),
        bad = show_bad_arg.get(args),
        dynamic_thys = show_dynamic_thys_arg.get(args),
        base_session_thys = show_base_session_thys_arg.get(args),
        session_dirs = show_session_dirs_arg.get(args),
        options = show_options_arg.get(args),
        statistics = show_statistics_arg.get(args))
      val long_running_threshold = PIDE_AI_Tool_Schema.long_running_arg.get(args)
      JSON_Object("sessions" -> selected_sessions.map(session =>
        PIDE_AI_Session_State.json(session, filter_nodes.get(session.id), show,
          long_running_threshold.getOrElse(session.long_running_threshold),
          context.cwd))) ++
      JSON_Object.if_proper(show.statistics,
        PIDE_AI_Session_State.jvm_stats_json())
    }
}

class Get_Session_State_Tools extends PIDE_AI_Tools(new Get_Session_State_Tool)
