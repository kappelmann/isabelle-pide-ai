/*  Title:      AI/src/Command/pide_ai_command.scala
    Author:     Kevin Kappelmann

Command connections: one command "TOOL [JSON]", answered by lines
{"progress": ...}, then {"result": ...} or {"error": ...}.
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Command {
  def request_line(tool_id: String, args: JSON.Object.T): String =
    tool_id + " " + JSON.Format(args)

  def parse_request(line: String): (String, JSON.Object.T) = {
    val (tool_id, rest) = line.span(_ != ' ')
    if (tool_id.isEmpty) error("Bad request: expected \"TOOL [JSON]\"")
    if (rest.isEmpty) (tool_id, JSON.Object.empty)
    else JSON.parse(rest.tail, strict = false) match {
      case JSON.Object(args) => (tool_id, args)
      case _ => error("Bad arguments: expected JSON object")
    }
  }

  val progress_response = JSON_Schema.Format.property("progress", JSON_Schema.Format.string)
  val result_response: JSON_Schema.Format[PIDE_AI_Tool_Result] =
    JSON_Schema.Format.result(JSON_Schema.Format.property("result", JSON_Schema.Format.any),
      JSON_Schema.Format.property("error", JSON_Schema.Format.any))
}
