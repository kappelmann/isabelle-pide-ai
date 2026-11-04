/*  Title:      AI/src/Tools/get_session_state_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

import scala.collection.immutable.VectorMap

class Get_Session_State_Tool extends PIDE_AI_Tool("get_session_state") {
  def description: String =
    "State of running sessions, information for their theories " +
    "(status, commands being processed,...), resource statistics,... " +
    "Sorrys are not listed."

  val filter_origins_arg = JSON_Schema.Arg.default(
    "filter_origins",
    "Session id to theory origins whose information is returned. " +
    "Default for entry-less session: all dynamic theories.",
    JSON_Schema.Format.dictionary(
      JSON_Schema.Format.list(JSON_Schema.Format.string)),
    VectorMap.empty)
  val include_cmds_processing_arg = JSON_Schema.Arg.default(
    "include_cmds_processing", "", JSON_Schema.Format.bool, true)
  val include_thy_status_arg = JSON_Schema.Arg.default(
    "include_thy_status", "Status, progress, command counts.",
    JSON_Schema.Format.bool, true)
  val include_loaded_thys_arg = JSON_Schema.Arg.default(
    "include_loaded_thys", "", JSON_Schema.Format.bool, false)
  val include_session_dirs_arg = JSON_Schema.Arg.default(
    "include_session_dirs", "Directories from which a session resolves files.",
    JSON_Schema.Format.bool, false)
  val include_options_arg = JSON_Schema.Arg.default(
    "include_options",
    "Isabelle session options. " +
    s"${quote("changed")} shows options that differ from their default. " +
    "true shows all.",
    PIDE_AI_Session_State.Include.Options.format,
    PIDE_AI_Session_State.Include.Options.`false`)
  val include_statistics_arg = JSON_Schema.Arg.default(
    "include_statistics",
    "Each session's ML and the server's JVM statistics (heap, threads,...).",
    JSON_Schema.Format.bool, false)

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(PIDE_AI_Tool_Schema.running_sessions_arg, filter_origins_arg,
      include_cmds_processing_arg, include_thy_status_arg, include_loaded_thys_arg,
      include_session_dirs_arg, include_options_arg, include_statistics_arg))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val selected_sessions = PIDE_AI_Tool_Util.running_sessions_param(server.sessions, args)
      val filter_origins = filter_origins_arg.get(args)
      val selected_ids = selected_sessions.map(_.id)
      val unknown = filter_origins.keysIterator.filterNot(selected_ids.toSet).toList
      if (unknown.nonEmpty) {
        error(s"Session(s) ${commas_quote(unknown)} of filter_origins not contained in " +
          s"selection ${commas_quote(selected_ids)}.")
      }
      val include = PIDE_AI_Session_State.Include(
        cmds_processing = include_cmds_processing_arg.get(args),
        thy_status = include_thy_status_arg.get(args),
        loaded_thys = include_loaded_thys_arg.get(args),
        session_dirs = include_session_dirs_arg.get(args),
        options = include_options_arg.get(args),
        statistics = include_statistics_arg.get(args))
      JSON_Object("sessions" -> selected_sessions.map(session =>
        PIDE_AI_Session_State.json(session, filter_origins.get(session.id), include))) ++
      JSON_Object.if_proper(include.statistics,
        PIDE_AI_Session_State.jvm_stats_json())
    }
}

class Get_Session_State_Tools extends PIDE_AI_Tools(new Get_Session_State_Tool)
