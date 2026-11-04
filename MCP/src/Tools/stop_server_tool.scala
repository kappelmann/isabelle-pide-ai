/*  Title:      MCP/src/Tools/stop_server_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object Stop_Server_Tool extends PIDE_MCP_Tool("stop_server") {
  def description: String = "Stop PIDE AI server."

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(Nil)

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result = {
    server.stop()
    Result.Res("Stopping server")
  }
}
