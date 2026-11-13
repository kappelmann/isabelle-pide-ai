/*  Title:      AI/src/PIDE/pide_ai_document_model.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Document_Model {
  def node_perspective(node: Document.Node): Document.Node.Perspective_Text.T =
    Document.Node.Perspective(
      node.perspective.required, node.text_perspective, node.perspective.overlays)

  object Replacement {
    def apply(old_text: String, new_text: String): Replacement = {
      val length = old_text.length min new_text.length
      var prefix = 0
      while (prefix < length && old_text(prefix) == new_text(prefix)) prefix += 1
      var suffix = 0
      while (suffix < length - prefix &&
        old_text(old_text.length - 1 - suffix) == new_text(new_text.length - 1 - suffix))
        suffix += 1
      Replacement(prefix, old_text.substring(prefix, old_text.length - suffix).nn,
        new_text.substring(prefix, new_text.length - suffix).nn)
    }
  }

  sealed case class Replacement(start: Text.Offset, old_text: String, new_text: String) {
    def edits: List[Text.Edit] = Text.Edit.replace(start, old_text, new_text)

    def restrict(perspective: Text.Perspective): Text.Perspective = {
      val before = Text.Range(0, start)
      val after = Text.Range(start + old_text.length, Text.Range.full.stop)
      val shift = new_text.length - old_text.length
      Text.Perspective(perspective.ranges.flatMap(range =>
        range.try_restrict(before).toList :::
        range.try_restrict(after).map(_ + shift).toList).filterNot(_.is_singularity))
    }
  }

  def hide_edits(
    nodes: Document.Nodes,
    keep: Set[Document.Node.Name]
  ): List[Document.Edit_Text] =
    nodes.iterator.collect[Document.Edit_Text] {
      case (name, node) if !keep(name) && !node.text_perspective.is_empty =>
        name -> node_perspective(node).copy(visible = Text.Perspective.empty)
    }.toList

  def unload_edits(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    node: Document.Node
  ): List[Document.Edit_Text] = {
    val model = PIDE_AI_Document_Model(session, node_name, node, "",
      Text.Edit.removes(0, node.source), Text.Perspective.empty)
    model.node_edits(node.thy, model.pending_edits, Document.Node.Perspective_Text.empty)
  }

  def make(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    node: Document.Node,
    text: String,
    visible_lines: Option[List[(Int, Option[Int])]],
    range_context: Int
  ): PIDE_AI_Document_Model = {
    val replacement = Replacement(node.source, text)
    val text_perspective = visible_lines match {
      case None => replacement.restrict(node.text_perspective)
      case Some(visible_lines) =>
        val doc = Line.Document(text)
        val line_ranges = visible_lines.map { (start_line, opt_end_line) =>
          (start_line, opt_end_line.getOrElse(doc.lines.length)) }
        PIDE_AI_Util.check_text_perspective(doc, line_ranges, range_context)
    }
    PIDE_AI_Document_Model(session, node_name, node, text, replacement.edits, text_perspective)
  }
}

sealed case class PIDE_AI_Document_Model(
  session: PIDE_AI_Session,
  node_name: Document.Node.Name,
  node: Document.Node,
  text: String,
  pending_edits: List[Text.Edit],
  text_perspective: Text.Perspective
) extends Document.Model {
  def node_required: Boolean = true
  def untyped_data: AnyRef = File_Format.registry.parse_data(node_name, text)

  def is_stable: Boolean = pending_edits.isEmpty

  def get_text(range: Text.Range): Option[String] = range.try_substring(text)

  def get_blob: Option[Document.Blobs.Item] =
    if (is_theory) None
    else Some(Document.Blobs.Item.make(
      Bytes(Symbol.encode(text)), source = Some(text), changed = !is_stable))

  def get_thy(): Resources.Thy = {
    val thy0 = session.resources.special_thy(node_name).getOrElse(
      session.resources.check_thy(node_name, Scan.char_reader(text)))
    thy0.eval_conditions(session.conditions)
  }

  def node_perspective: Document.Node.Perspective_Text.T =
    if (is_theory)
      PIDE_AI_Document_Model.node_perspective(node).copy(
        required = node_required, visible = text_perspective)
    else Document.Node.Perspective_Text.empty

  def edits: List[Document.Edit_Text] =
    if (is_stable &&
        (if (is_theory)
          session.resources.is_base_session_thy(node_name) ||
          node.edit_perspective == node_perspective
        else node.get_blob.isDefined)) Nil
    else node_edits(get_thy(), pending_edits, node_perspective)
}
