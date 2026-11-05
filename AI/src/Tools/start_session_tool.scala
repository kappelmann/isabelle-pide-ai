/*  Title:      AI/src/Tools/start_session_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object Start_Session_Tool extends PIDE_AI_Tool("start_session") {
  def description: String =
    "Start a session. Returns its id once ready. " +
    "Builds the base session if needed (may take long). " +
    PIDE_AI_Tool_Schema.session_restart

  def input_schema: JSON.Object.T = 
    JSON_Schema.object_schema(PIDE_AI_Session_Spec.json_args)

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val spec = PIDE_AI_Session_Spec.from_json(args)
      val session =
        server.sessions.start_session_reporting_build_status(Options.init(), spec, progress)
      JSON_Object("session" -> session.id)
    }
}

class Start_Session_Tools extends PIDE_AI_Tools(Start_Session_Tool)
