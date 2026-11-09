/*  Title:      AI/src/Command/pide_ai_command.scala
    Author:     Kevin Kappelmann

Command connections: one request line with the tool call params of the Model Context Protocol
{"name": TOOL, "arguments": {...}, "_meta": {"progressToken": TOKEN}}, answered by lines
{"progress": ...} if requested, then {"result": ...} or {"error": ...}.
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Command {
  def parse_tool_call(line: String): MCP.Tool_Call =
    JSON.parse(line, strict = false) match {
      case JSON.Object(params) => MCP.read_tool_call(params)
      case _ => error("Bad request: expected JSON object")
    }

  val progress_response = JSON_Schema.Format.property("progress", JSON_Schema.Format.string)
  val result_response: JSON_Schema.Format[PIDE_AI_Tool_Result] =
    JSON_Schema.Format.result(JSON_Schema.Format.property("result", JSON_Schema.Format.any),
      JSON_Schema.Format.property("error", JSON_Schema.Format.any))
}
