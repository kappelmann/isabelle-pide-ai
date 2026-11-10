/*  Title:      AI/src/General/pide_ai_util.scala
    Author:     Kevin Kappelmann

General utilities.
*/

package isabelle.pide.ai

import isabelle._
import isabelle.symbols.Symbols

object PIDE_AI_Util {
  def log_failure(log: Logger, what: String, result: Exn.Result[?]): Unit =
    result match {
      case Exn.Exn(exn) => Exn.capture { log.error_message(s"$what: ${Exn.print(exn)}") }
      case Exn.Res(_) =>
    }

  def handle_internal_error(log: Logger, what: String, exn: Throwable): String = {
    log_failure(log, "Internal error for " + what, Exn.Exn(exn))
    "Internal error: " + Exn.message(exn)
  }

  def combined_failure(exn: Throwable, more: List[Throwable]): Throwable =
    (exn :: more).filterNot(Exn.is_interrupt) match {
      case Nil => exn
      case List(exn1) => exn1
      case failures => ERROR(cat_lines(failures.map(Exn.message)))
    }

  // like Exn.release_first, but reports every error instead of only the first one
  def release_combined(results: List[Exn.Result[?]]): Unit =
    results.collect { case Exn.Exn(exn) => exn } match {
      case exn :: more => throw combined_failure(exn, more)
      case Nil =>
    }

  def text_range(doc: Line.Document, start_line: Int, end_line: Int): Option[Text.Range] =
    if (start_line - 1 > end_line) None
    else doc.text_range(Line.Range(Line.Position(start_line - 1), Line.Position(end_line)))

  def range_lines(doc: Line.Document, range: Text.Range): (Int, Int) =
    (doc.position(range.start).line1, doc.position(range.start max (range.stop - 1)).line1)

  def check_ordered_lines(start_line: Int, end_line: Option[Int]): Unit =
    for (stop <- end_line if stop < start_line) error(s"end_line $stop < start_line $start_line")

  def check_lines(start_line: Int, end_line: Option[Int], total_lines: Int): (Int, Int) = {
    def check_positive(name: String, line: Int): Unit =
      if (line < 1) error(s"$name $line out of bounds (lines start at 1)")
    check_ordered_lines(start_line, end_line)
    check_positive("start_line", start_line)
    if (start_line > (total_lines max 1))
      error(s"start_line $start_line out of bounds (file has $total_lines lines)")
    end_line.foreach(check_positive("end_line", _))
    (start_line, end_line.fold(total_lines)(_ min total_lines))
  }

  def check_text_range(doc: Line.Document, start_line: Int, end_line: Option[Int]): Text.Range = {
    val (first_line, last_line) = check_lines(start_line, end_line, doc.lines.length)
    text_range(doc, first_line, last_line).get
  }

  def check_text_perspective(
    doc: Line.Document,
    line_ranges: List[(Int, Int)],
    context: Int
  ): Text.Perspective =
    Text.Perspective(line_ranges.map { (start_line, end_line) =>
      text_range(doc, (start_line - context) max 1, (end_line + context) min doc.lines.length)
        .getOrElse(error(
          s"Bad line range $start_line..$end_line (file has ${doc.lines.length} lines)")) })

  def numbered_line(line: Int, text: String): String =
    s"${line}: ${text}"

  def line_range(start_line: Int, end_line: Int): String =
    if (start_line == end_line) start_line.toString else s"$start_line-$end_line"

  def line_ranges_span(line_ranges: List[(Int, Int)]): Option[(Int, Int)] =
    Option.when(line_ranges.nonEmpty)((line_ranges.head._1, line_ranges.last._2))

  def numbered_lines(lines: IterableOnce[String], start_line: Int): String =
    lines.iterator.zipWithIndex.map { (l, i) =>
      numbered_line(start_line + i, l) }.mkString("\n")

  def numbered_lines(text: String, start_line: Int): String =
    numbered_lines(Line.logical_lines(text), start_line)

  def numbered_lines_range(doc: Line.Document, start_line: Int, end_line: Int): String =
    numbered_lines(doc.lines.iterator.slice(start_line - 1, end_line).map(_.text), start_line)

  def snippet(doc: Line.Document, range: Text.Range, snippet_lines: Int): String = {
    val (start, stop) = range_lines(doc, range)
    val end = stop min (start + snippet_lines - 1)
    val shown = numbered_lines_range(doc, start, end)
    val omitted = Option.when(end < stop)(line_range(end + 1, stop) + ": ...")
    cat_lines(proper_string(shown).toList ::: omitted.toList)
  }

  def print_process_result(process_result: Process_Result): String =
    process_result.print_rc + if_proper(process_result.err, s": ${process_result.err}")

  def path(s: String, cwd: Option[Path]): Path = {
    val path = Path.explode(File.standard_path(s))
    cwd.fold(path)(_ + path)
  }

  def relative_path(path: Path, cwd: Option[Path]): Path = {
    val abs = path.absolute
    cwd.flatMap(File.get_relative_path(_, abs)).getOrElse(abs)
  }

  def print_path(path: Path, cwd: Option[Path]): String = relative_path(path, cwd).implode

  def absolute_file(file: String): Option[Path] =
    try { Some(path(file, None).expand).filter(_.is_absolute) }
    catch { case ERROR(_) => None }

  def print_file(file: String, cwd: Option[Path]): String =
    absolute_file(file).map(print_path(_, cwd)).getOrElse(file)

  def display_name(
    entry: Option[Name_Space.Entry],
    range: Text.Range,
    source: String,
    unicode_symbols: Boolean
  ): String =
    entry.map(_.name).filter(_.nonEmpty).map(Symbols.output_decoded(unicode_symbols, _))
      .getOrElse(range.substring(source))

  def intersect_range(full: Text.Range, range: Option[Text.Range]): Text.Range =
    range.fold(full)(r => full.try_restrict(r).getOrElse(Text.Range.zero))

  def restrict_text_range(text: String, range: Option[Text.Range]): Text.Range =
    intersect_range(Text.Range.length(text), range)

  def restrict_ranges(ranges: Iterable[Text.Range], range: Text.Range): List[Text.Range] =
    ranges.iterator.filter(_.overlaps(range)).map(_.restrict(range)).toList

  def elem_body_plain_text(elem: XML.Elem, unicode_symbols: Boolean): String =
    Symbols.output_decoded(unicode_symbols, Pretty.string_of(elem.body, pure = true))

  def xml_json(tree: XML.Tree, unicode_symbols: Boolean): JSON.Object.T =
    tree match {
      case XML.Elem(Markup(name, props), body) =>
        val props_obj =
          JSON_Object(props.map((a, b) => a -> Symbols.output_decoded(unicode_symbols, b)))
        val base = JSON_Object("name" -> name, "body" -> body.map(xml_json(_, unicode_symbols)))
        if (props_obj.isEmpty) base else base + ("props" -> props_obj)
      case XML.Text(text) => JSON_Object("text" -> Symbols.output_decoded(unicode_symbols, text))
    }
}
