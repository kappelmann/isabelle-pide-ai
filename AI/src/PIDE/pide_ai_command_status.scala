/*  Title:      AI/src/PIDE/pide_ai_command_status.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Command_Status {
  def command_status(snapshot: Document.Snapshot, cmd: Command): Document_Status.Command_Status =
    snapshot.state.command_status(snapshot.version, cmd)

  // cf. Rendering.overview_color
  def range_command_status(
    snapshot: Document.Snapshot,
    range: Text.Range
  ): Document_Status.Command_Status = {
    val markups = snapshot.cumulate[List[Markup]](range, Nil,
      Document_Status.Command_Status.liberal_elements,
      _ => { case (status, Text.Info(_, elem)) => Some(elem.markup :: status) }, status = true)
    Document_Status.Command_Status.make(Date.now(), markups = markups.flatMap(_.info))
  }

  def statuses(status: Document_Status.Command_Status): List[PIDE_AI_Command_Status] =
    values.toList.filter(_.is_status(status))

  def statuses(snapshot: Document.Snapshot, cmd: Command): List[PIDE_AI_Command_Status] =
    statuses(command_status(snapshot, cmd))

  val problematic: Set[PIDE_AI_Command_Status] = Set(failed, warned, canceled)
}

enum PIDE_AI_Command_Status(
  val name: String,
  val is_status: Document_Status.Command_Status => Boolean,
  val message_kind: Option[PIDE_AI_Message.Kind] = None
) {
  // "processing" because "running" confuses agents: "running" = "it worked"
  case running extends PIDE_AI_Command_Status("processing", _.is_running)
  case failed extends PIDE_AI_Command_Status("failed", _.is_failed,
    Some(PIDE_AI_Message.Kind.error))
  case warned extends PIDE_AI_Command_Status("warned", _.is_warned,
    Some(PIDE_AI_Message.Kind.warning))
  case canceled extends PIDE_AI_Command_Status("canceled", _.is_canceled)
  case unprocessed extends PIDE_AI_Command_Status("unprocessed", _.is_unprocessed)
  case finished extends PIDE_AI_Command_Status("finished", _.is_finished)
}
