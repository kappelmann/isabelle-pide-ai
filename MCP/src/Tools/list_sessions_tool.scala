/*  Title:      MCP/src/Tools/list_sessions_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

class List_Sessions_Tool extends PIDE_MCP_Tool("list_sessions") {
  def description: String = "List all sessions."
  def input_schema: JSON.Object.T = JSON_Schema.object_schema(Nil)
  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.result { server.sessions.state_json() }
}

class List_Sessions_Tools extends PIDE_MCP_Tools(new List_Sessions_Tool)
