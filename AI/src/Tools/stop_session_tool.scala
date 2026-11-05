/*  Title:      AI/src/Tools/stop_session_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

class Stop_Session_Tool extends PIDE_AI_Tool("stop_session") with PIDE_AI_Tool.Destructive {
  def description: String = "Stop sessions. " +
    PIDE_AI_Tool_Schema.session_restart

  val sessions_arg = JSON_Schema.Arg.optional(
    "sessions", "Stoppable session ids. Default: all stoppable sessions.",
    JSON_Schema.Format.list(JSON_Schema.Format.string))

  def input_schema: JSON.Object.T =
    JSON_Schema.object_schema(List(sessions_arg))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val stopped = server.sessions.stop_sessions(sessions_arg.get(args), progress)
      JSON_Object("stopped" -> stopped, "remaining" -> server.sessions.state_json())
    }
}

class Stop_Session_Tools extends PIDE_AI_Tools(new Stop_Session_Tool)
