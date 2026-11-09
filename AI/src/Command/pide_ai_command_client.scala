/*  Title:      AI/src/Command/pide_ai_command_client.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Command_Client {
  def read_response(
    connection: Line_Connection,
    tool_id: String,
    progress: PIDE_AI_Progress
  ): PIDE_AI_Tool_Result = {
    var result: Option[PIDE_AI_Tool_Result] = None
    for (line <- connection.read_lines()) {
      def bad(): Nothing = error(s"Bad response to ${quote(tool_id)}: $line")
      if (result.isDefined) bad()
      JSON.Format.unapply(line) match {
        case Some(PIDE_AI_Command.progress_response(message)) => progress.echo_json(message)
        case Some(PIDE_AI_Command.result_response(res)) => result = Some(res)
        case _ => bad()
      }
    }
    result.getOrElse(error(s"No response to ${quote(tool_id)}"))
  }

  def call_tool(
    connection: Line_Connection,
    tool_id: String,
    args: JSON.Object.T,
    progress: PIDE_AI_Progress
  ): PIDE_AI_Tool_Result = {
    connection.write_line(JSON.Format(MCP.Tool_Call(tool_id, args, Some(1L)).json))
    read_response(connection, tool_id, progress)
  }
}
