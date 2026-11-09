/*  Title:      AI/src/PIDE/pide_ai_message.scala
    Author:     Kevin Kappelmann

Message utilities.
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Message {
  object Kind {
    val problematic: Set[Kind] = Set(error, warning)

    def of(elem: XML.Elem): Option[Kind] = values.find(_.is_kind(elem))

    require(Markup.messages.keySet.forall(name => of(XML.Elem(Markup(name, Nil), Nil)).isDefined),
      "Unclassified message kinds")
  }

  enum Kind(val is_kind: XML.Elem => Boolean) {
    case state extends Kind(Protocol.is_state)
    case error extends Kind(Protocol.is_error)
    case warning extends Kind(Protocol.is_warning_or_legacy)
    case writeln extends Kind(Protocol.is_writeln)
    case information extends Kind(Protocol.is_information)
    case tracing extends Kind(Protocol.is_tracing)
  }

  def output_message(
    snapshot: Document.Snapshot,
    cmd: Command,
    msg: XML.Elem,
    cwd: Option[Path]
  ): XML.Elem =
    XML.Elem(msg.markup, msg match {
        case Protocol.ML_Profiling(report) => XML.string(report.print)
        case _ => PIDE_AI_Document_Command.resolve_positions(snapshot, cmd, msg.body, cwd)
      })

  object Messages {
    def apply(
      snapshot: Document.Snapshot,
      cmd: Command,
      command_start: Text.Offset,
      range: Text.Range
    ): List[PIDE_AI_Message] = {
      def message_ranges(elem: XML.Elem): List[Text.Range] = {
        val ranges = PIDE_AI_Document_Command.elem_positions(snapshot, cmd, elem)
        if (ranges.isEmpty) List(range)
        else {
          val sorted = ranges.toList.sortBy(_.start).map(_ + command_start)
          PIDE_AI_Util.restrict_ranges(sorted, range)
        }
      }
      snapshot.command_results(cmd).iterator.flatMap((_, elem) =>
        for {
          kind <- Kind.of(elem)
          ranges = message_ranges(elem) if ranges.nonEmpty
        } yield PIDE_AI_Message(kind, elem, ranges, snapshot, Some(cmd))).toList
    }

    def key(kind: Kind): String =
      kind match {
        case Kind.state => "goals"
        case Kind.error => "errors"
        case Kind.warning => "warnings"
        case Kind.writeln | Kind.information | Kind.tracing => kind.toString
      }

    def json(
      messages: Map[Kind, List[PIDE_AI_Message]],
      kinds: List[Kind],
      unicode_symbols: Boolean,
      cwd: Option[Path]
    ): List[Option[JSON.Object.Entry]] =
      kinds.map(kind => messages.get(kind).map(msgs =>
        key(kind) -> msgs.map(_.text(unicode_symbols, cwd))))
  }
}

sealed case class PIDE_AI_Message(
  kind: PIDE_AI_Message.Kind,
  elem: XML.Elem,
  ranges: List[Text.Range],
  snapshot: Document.Snapshot,
  cmd: Option[Command]
) {
  def text(unicode_symbols: Boolean, cwd: Option[Path]): String =
    PIDE_AI_Util.elem_body_plain_text(
      cmd.fold(elem)(PIDE_AI_Message.output_message(snapshot, _, elem, cwd)), unicode_symbols)
}
