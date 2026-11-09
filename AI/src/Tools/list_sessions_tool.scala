/*  Title:      AI/src/Tools/list_sessions_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

class List_Sessions_Tool extends PIDE_AI_Tool("list_sessions") with PIDE_AI_Tool.Read_Only {
  def description: String = "List all sessions."
  def input_schema: JSON.Object.T = JSON_Schema.object_schema(Nil)

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: PIDE_AI_Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result { server.sessions.state_json() }
}

class List_Sessions_Tools extends PIDE_AI_Tools(new List_Sessions_Tool)
