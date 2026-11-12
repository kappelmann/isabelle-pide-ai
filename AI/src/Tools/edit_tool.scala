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
    edit_all: Boolean,
    sync: PIDE_AI_Tool_Util.Sync)

  def load_edit(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    request: Request,
    progress: PIDE_AI_Progress
  ): List[(Int, Int)] = {
    PIDE_AI_Util.check_ordered_lines(request.start_line, request.end_line)
    session.with_lock(progress) {
      val current_text = PIDE_AI_Tool_Util.sync_load_missing_nodes(session,
        List(node_name -> None), request.sync, hide_others = false, progress)._1(node_name)
      val search_range = PIDE_AI_Util.check_text_range(Line.Document(current_text),
        request.start_line, request.end_line)
      val range_text = search_range.substring(current_text)
      val old_text = request.old_text
      val text = request.new_text
      val new_text =
        if (old_text.nonEmpty || text.isEmpty || text.endsWith("\n")) text
        else if (range_text.endsWith("\n") || request.mode == Mode.prepend) text + "\n"
        else if (request.mode == Mode.append && range_text.nonEmpty) "\n" + text
        else text
      val actual_old_text = if (old_text.isEmpty) range_text else old_text
      val offsets = if (old_text.isEmpty) List(search_range.start)
        else {
          val matches = new Regex(Regex.quote(actual_old_text))
            .findAllMatchIn(range_text).map(_.start + search_range.start).toList
          if (matches.isEmpty)
            error("old_text not found in given range. The file may have changed.")
          if (!request.edit_all && matches.length > 1)
            error(s"Found ${matches.length} matches of old_text in given range. " +
              "Expected exactly 1. " +
              "Provide more context (larger old_text), restrict the range, or use edit_all.")
          if (request.edit_all) matches else List(matches.head)
        }
      val replaced_ranges = offsets.map(replaced_range(request.mode, _, actual_old_text.length))
      val computed_text = Library.string_builder(current_text.length) { text =>
        var start = 0
        for (range <- replaced_ranges) {
          text ++= current_text.substring(start, range.start).nn
          text ++= new_text
          start = range.stop
        }
        text ++= current_text.substring(start).nn
      }
      val changed = computed_text != current_text
      val edited_doc = Line.Document(computed_text)
      val edited_lines = replaced_ranges.zipWithIndex.map { (range, i) =>
        val start = range.start + i * (new_text.length - range.length)
        PIDE_AI_Util.range_lines(edited_doc, Text.Range(start, start + new_text.length))
      }
      progress.expose_interrupt()
      if (changed) session.resources.write_file_content(node_name.path, computed_text)
      val (_, change) = session.resources.load_nodes(session,
        List(node_name -> Some(edited_lines.map(_ -> Some(_)))),
        hide_others = true,
        // avoid disk <--> PIDE disagreement
        new Unstoppable_Progress(progress))
      session.await_stable(change, progress)
      session.resources.load_missing(session, List(node_name), progress)
      if (changed) edited_lines else Nil
    }
  }

  enum Edited_Lines { case none, each, bounding_box }

  val base_description = "Edit matching text in a given range."

  val mode_arg = JSON_Schema.Arg.default("mode", "",
    JSON_Schema.Format.enumeration(Mode.values), Mode.replace)
  val text_arg = JSON_Schema.Arg.required(
    "text", "Text to write.", JSON_Schema.Format.string)
  val old_text_arg = JSON_Schema.Arg.required(
    "old_text", "Text to find. If empty, all lines in range are selected.",
    JSON_Schema.Format.string)
  val edit_all_arg = JSON_Schema.Arg.default(
    "edit_all", "Edit every match or a unique occurrence.", JSON_Schema.Format.bool,
    false)
  val show_edited_lines_arg = JSON_Schema.Arg.default(
    "show_edited_lines", "",
    JSON_Schema.Format.enumeration(Edited_Lines.values), Edited_Lines.bounding_box)

  val edit_args: List[JSON_Schema.Arg[?]] = List(PIDE_AI_Tool_Schema.running_session_arg,
    PIDE_AI_Tool_Schema.node_arg, mode_arg, text_arg,
    PIDE_AI_Tool_Schema.start_line_default_arg, PIDE_AI_Tool_Schema.opt_end_line_arg,
    old_text_arg, edit_all_arg, show_edited_lines_arg)

  def load_edit_json(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    args: JSON.Object.T,
    sync: PIDE_AI_Tool_Util.Sync,
    progress: PIDE_AI_Progress
  ): (List[(Int, Int)], JSON.Object.T) = {
    def pide_text(arg: JSON_Schema.Arg[String]): String =
      session.resources.pide_text(node_name.path, Line.normalize(arg.get(args)))
    val request = Request(mode_arg.get(args), pide_text(text_arg),
      PIDE_AI_Tool_Schema.start_line_default_arg.get(args),
      PIDE_AI_Tool_Schema.opt_end_line_arg.get(args), pide_text(old_text_arg),
      edit_all_arg.get(args), sync)
    val show_edited_lines = show_edited_lines_arg.get(args)
    val edited_lines = load_edit(session, node_name, request, progress)
    val shown_lines = show_edited_lines match {
      case Edited_Lines.none => None
      case Edited_Lines.bounding_box => PIDE_AI_Util.line_ranges_span(edited_lines)
        .map(PIDE_AI_Util.line_range)
      case Edited_Lines.each => proper_list(edited_lines.map(PIDE_AI_Util.line_range))
    }
    val json = JSON_Object("edits" -> edited_lines.length) ++
      JSON.optional("edited_lines" -> shown_lines) ++ JSON_Object.if_proper(edited_lines.isEmpty,
        JSON_Object("note" -> "The edit left the text unchanged."))
    (edited_lines, json)
  }
}

class Edit_Tool extends PIDE_AI_Tool("edit") with PIDE_AI_Tool.Destructive {
  def description: String =
    Edit_Tool.base_description + " " + PIDE_AI_Tool_Schema.range_perspective + " " +
    PIDE_AI_Tool_Schema.no_processing_wait + " " +
    PIDE_AI_Tool_Schema.implicit_load_files

  def input_schema: JSON.Object.T =
    JSON_Schema.object_schema(Edit_Tool.edit_args :+ PIDE_AI_Tool_Schema.node_disk_sync_arg)

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: PIDE_AI_Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val node_name = PIDE_AI_Tool_Util.node_param(session, args, context.cwd)
      Edit_Tool.load_edit_json(session, node_name, args,
        PIDE_AI_Tool_Schema.node_disk_sync_arg.get(args), progress)._2
    }
}

class Edit_Tools extends PIDE_AI_Tools(new Edit_Tool)
