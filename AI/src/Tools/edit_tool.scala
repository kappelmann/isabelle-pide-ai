/*  Title:      AI/src/Tools/edit_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

import scala.util.matching.Regex

object Edit_Tool {
  enum Mode { case replace, prepend, append }

  def replaced_range(mode: Mode, offset: Text.Offset, old_length: Int): Text.Range =
    mode match {
      case Mode.replace => Text.Range(offset, offset + old_length)
      case Mode.prepend => Text.Range(offset)
      case Mode.append => Text.Range(offset + old_length)
    }

  sealed case class Request(
    mode: Mode,
    new_text: String,
    start_line: Int,
    end_line: Option[Int],
    old_text: String,
    edit_all: Boolean)

  def edit(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    request: Request,
    progress: Progress,
    cwd: Option[Path]
  ): List[(Int, Int)] = {
    if (session.is_base_session_theory(node_name))
      error(s"Cannot edit base session theory ${quote(session.print_origin(node_name, cwd))}")
    PIDE_AI_Util.check_ordered_lines(request.start_line, request.end_line)
    session.with_lock(progress) {
      val current_text = session.read_update(
        List(node_name -> None), hide_others = false, progress = progress)._1(node_name)
      val search_range = PIDE_AI_Util.check_text_range(Line.Document(current_text),
        request.start_line, request.end_line)
      val range_text = search_range.substring(current_text)
      val old_text = request.old_text
      val new_text = request.new_text
      val actual_old_text = if (old_text.isEmpty) range_text else old_text
      val offsets = if (old_text.isEmpty) List(search_range.start)
        else {
          val occurrences = new Regex(Regex.quote(actual_old_text))
            .findAllMatchIn(range_text).map(_.start + search_range.start).toList
          if (occurrences.isEmpty)
            error("old_text not found in given range. The file may have changed.")
          if (!request.edit_all && occurrences.length > 1)
            error(s"Found ${occurrences.length} occurrences of old_text in given range. " +
              "Expected exactly 1. " +
              "Provide more context (larger old_text), restrict the range, or use edit_all.")
          if (request.edit_all) occurrences else List(occurrences.head)
        }
      val replaced_ranges = offsets.map(replaced_range(request.mode, _, actual_old_text.length))
      val computed_text = replaced_ranges.reverseIterator.foldLeft(current_text) { (text, range) =>
        text.take(range.start) + new_text + text.drop(range.stop)
      }
      val changed = computed_text != current_text
      val edited_doc = Line.Document(computed_text)
      val edited_lines = replaced_ranges.zipWithIndex.map { case (range, i) =>
        val start = range.start + i * (new_text.length - range.length)
        val line_range = edited_doc.range(Text.Range(start, start + new_text.length))
        (line_range.start.line1, line_range.stop.line1)
      }
      progress.expose_interrupt()
      if (changed) session.write_file_content(node_name.path, computed_text)
      val (_, change) = session.read_update(
        List(node_name -> Some(edited_lines.map(_ -> Some(_)))),
        hide_others = true,
        // avoid disk <--> PIDE disagreement
        progress = new Unstoppable_Progress(progress))
      session.await_stable(change, progress)
      session.resolve_dependencies(List(node_name), progress)
      if (changed) edited_lines else Nil
    }
  }

  enum Edited_Lines { case none, each, range }

  val base_description ="Edit matching text in a given range."

  val mode_arg = JSON_Schema.Arg.default("mode", "",
    JSON_Schema.Format.enumeration(Mode.values), Mode.replace)
  val text_arg = JSON_Schema.Arg.required(
    "text", "Text to write.", JSON_Schema.Format.string)
  val old_text_arg = JSON_Schema.Arg.required(
    "old_text", "Text to find. If empty, whole text in range is selected.",
    JSON_Schema.Format.string)
  val edit_all_arg = JSON_Schema.Arg.default(
    "edit_all", "Edit every match or a unique occurrence.", JSON_Schema.Format.bool,
    false)
  val edited_lines_arg = JSON_Schema.Arg.default(
    "edited_lines", "Return format.", JSON_Schema.Format.enumeration(Edited_Lines.values),
    Edited_Lines.range)

  val edit_args: List[JSON_Schema.Arg[?]] = List(PIDE_AI_Tool_Schema.running_session_arg,
    PIDE_AI_Tool_Schema.origin_arg, mode_arg, text_arg,
    PIDE_AI_Tool_Schema.start_line_default_arg, PIDE_AI_Tool_Schema.opt_end_line_arg,
    old_text_arg, edit_all_arg, edited_lines_arg)

  def read_update_edit(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    args: JSON.Object.T,
    progress: Progress,
    cwd: Option[Path]
  ): (List[(Int, Int)], JSON.Object.T) = {
    def pide_text(arg: JSON_Schema.Arg[String]): String =
      session.pide_text(node_name.path, Line.normalize(arg.get(args)))
    val request = Request(mode_arg.get(args), pide_text(text_arg),
      PIDE_AI_Tool_Schema.start_line_default_arg.get(args),
      PIDE_AI_Tool_Schema.opt_end_line_arg.get(args), pide_text(old_text_arg),
      edit_all_arg.get(args))
    val edited_lines = edit(session, node_name, request, progress, cwd)
    val lines = edited_lines_arg.get(args) match {
      case Edited_Lines.none => None
      case Edited_Lines.range => PIDE_AI_Util.line_ranges_span(edited_lines)
        .map(PIDE_AI_Util.line_range)
      case Edited_Lines.each => proper_list(edited_lines.map(PIDE_AI_Util.line_range))
    }
    val json = JSON_Object("edits" -> edited_lines.length) ++
      JSON.optional("edited_lines" -> lines) ++ JSON_Object.if_proper(edited_lines.isEmpty,
        JSON_Object("note" -> "The edit left the text unchanged."))
    (edited_lines, json)
  }
}

class Edit_Tool extends PIDE_AI_Tool("edit") with PIDE_AI_Tool.Destructive {
  def description: String =
    Edit_Tool.base_description +" " + PIDE_AI_Tool_Schema.range_perspective + " " +
    PIDE_AI_Tool_Schema.no_processing_wait + " " +
    PIDE_AI_Tool_Schema.implicit_reload_file

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(Edit_Tool.edit_args)

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val node_name = PIDE_AI_Tool_Util.origin_param(session, args, context.cwd)
      Edit_Tool.read_update_edit(session, node_name, args, progress, context.cwd)._2
    }
}

class Edit_Tools extends PIDE_AI_Tools(new Edit_Tool)
