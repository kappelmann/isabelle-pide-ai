/*  Title:      Tool_Extensions/src/sledgehammer_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai.extensions

import isabelle._
import isabelle.pide.ai._

class Sledgehammer_Tool extends PIDE_AI_Tool("sledgehammer") {
  def description: String =
    "Run sledgehammer on command's goal at the given line without modifying the file. " +
    PIDE_AI_Tool_Schema.implicit_load_file

  val line_arg = JSON_Schema.Arg.required(
    "line", "", JSON_Schema.Format.int(minimum = Some(1)))
  val prefix_arg = JSON_Schema.Arg.optional(
    "prefix",
    "Prefix of the line. Sledgehammer runs on the command following it. Default: end of line.",
    JSON_Schema.Format.string)
  val provers_arg = JSON_Schema.Arg.optional(
    "provers",
    "Space-separated provers to run. Empty for Sledgehammer's default provers. " +
    "Default: sledgehammer_provers option.",
    JSON_Schema.Format.string)
  val isar_proofs_arg = JSON_Schema.Arg.default(
    "isar_proofs", "Also generate structured Isar proofs.", JSON_Schema.Format.bool, false)
  val try0_arg = JSON_Schema.Arg.default(
    "try0", "Try integrated provers with found facts before external ones.",
    JSON_Schema.Format.bool, true)
  val proofs_limit_arg = JSON_Schema.Arg.default(
    "proofs_limit", "Proofs to be found.", JSON_Schema.Format.int(minimum = Some(1)), 1)
  val timeout_arg = JSON_Schema.Arg.optional(
    "timeout", "Seconds before cancellation.",
    JSON_Schema.Format.int(minimum = Some(1)).map(Time.seconds(_), _.seconds.toInt))

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(PIDE_AI_Tool_Schema.running_session_arg, PIDE_AI_Tool_Schema.origin_arg,
      line_arg, prefix_arg, provers_arg, isar_proofs_arg, try0_arg,
      proofs_limit_arg, timeout_arg))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  private def caret_offset(
    source: String,
    line: Int,
    prefix: Option[String]
  ): Text.Offset = {
    val doc = Line.Document(source)
    val range = PIDE_AI_Util.check_text_range(doc, line, Some(line))
    val text = range.substring(source).stripLineEnd
    val caret = prefix match {
        case None => text.length
        case Some(p) =>
          if (!text.startsWith(p))
            error(s"Line $line does not start with ${quote(p)}. Actual line: ${quote(text)}")
          p.length
      }
    range.start + caret
  }

  private def proofs_count(output: Editor.Output): Int =
    output.messages.count(msg => Protocol.sendback_snippets(List(msg)).nonEmpty)

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val node_name = PIDE_AI_Tool_Util.origin_param(session, args)
      val snapshot = PIDE_AI_Tool_Util.require_stable_origin_snapshot(session, node_name, progress)
      val line = line_arg.get(args)
      val context = PIDE_AI_Editor.Context(node_name,
        caret_offset(snapshot.node.source, line, prefix_arg.get(args)))
      val provers = provers_arg.get(args).getOrElse(
        session.options.string("sledgehammer_provers"))
      val query_args = List(provers,
        Value.Boolean(isar_proofs_arg.get(args)), Value.Boolean(try0_arg.get(args)))
      val proofs_limit = proofs_limit_arg.get(args)
      new PIDE_AI_Query(session, context, "sledgehammer", progress)
        .run(query_args, timeout_arg.get(args),
          stop = output => proofs_count(output) >= proofs_limit)
        .json(PIDE_AI_Message.Message_Kind.values.toList)
    }
}

class Sledgehammer_Tools extends PIDE_AI_Tools(new Sledgehammer_Tool)
