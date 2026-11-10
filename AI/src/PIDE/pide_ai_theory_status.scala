/*  Title:      AI/src/PIDE/pide_ai_theory_status.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

import scala.collection.mutable

object PIDE_AI_Theory_Status {
  // cf. Document_Status.Node_Status.make, Document_Status.Nodes_Status.overall_status
  def apply(snapshot: Document.Snapshot, name: Document.Node.Name): PIDE_AI_Theory_Status = {
    val node = snapshot.version.nodes(name)
    var theory_status = Document_Status.Theory_Status.NONE
    var total = 0
    var timing_ms = 0L
    val now = Date.now()
    val counts = mutable.Map[PIDE_AI_Command_Status, Int]().withDefaultValue(0)
    for (cmd <- node.commands.iterator) {
      val status = PIDE_AI_Command_Status.command_status(snapshot, cmd)
      theory_status = Document_Status.Theory_Status.merge(theory_status, status.theory_status)
      if (!PIDE_AI_Document_Command.is_ignorable(snapshot, cmd)) {
        total += 1
        timing_ms += status.timings.sum(now).ms
        PIDE_AI_Command_Status.statuses(status).foreach(counts(_) += 1)
      }
    }

    def percent(a: Int, b: Int): Int =
      if (b == 0) 0 else ((a.toDouble / b) * 100).toInt

    val consolidated = Document_Status.Theory_Status.consolidated(theory_status)
    val percentage = node.get_theory match {
      case None => if (consolidated) 100
        else percent(total - counts(PIDE_AI_Command_Status.unprocessed), total).min(99)
      case Some(command) =>
        val processed =
          PIDE_AI_Command_Status.command_status(snapshot, command).timings.count_finished
        percent(processed, command.span.theory_commands)
    }
    val overall_status = if (!consolidated) Document_Status.Overall_Status.pending
      else if (counts(PIDE_AI_Command_Status.failed) == 0)
        Document_Status.Overall_Status.ok
      else Document_Status.Overall_Status.failed
    val header_errors = PIDE_AI_Nodes.header_state(snapshot.switch(name))
      .filter(_.command_status.is_failed).toList.flatMap(_.errors)
    PIDE_AI_Theory_Status(name, overall_status, timing_ms, percentage, header_errors,
      counts.toMap)
  }

  object Statuses {
    enum Show {
      case none, names, failed, not_ok, all
      def contains(overall_status: Document_Status.Overall_Status): Boolean =
        this match {
          case `none` => false
          case `failed` => overall_status == Document_Status.Overall_Status.failed
          case `not_ok` => overall_status != Document_Status.Overall_Status.ok
          case `names` | `all` => true
        }
    }

    def apply(
      snapshot: Document.Snapshot,
      names: List[Document.Node.Name]
    ): List[PIDE_AI_Theory_Status] =
      names.map(PIDE_AI_Theory_Status(snapshot, _))

    def thys_key(overall_status: Document_Status.Overall_Status): String = "thys_" + overall_status

    def json(
      resources: PIDE_AI_Resources,
      statuses: List[PIDE_AI_Theory_Status],
      show: Show,
      cwd: Option[Path]
    ): JSON.Object.T = {
      def print_status_node_name(status: PIDE_AI_Theory_Status): String =
        resources.print_node_name(status.node_name, cwd)
      val by_status = statuses.groupMapReduce(_.overall_status)(_ => 1)(_ + _)
      val shown = statuses.filter(status => show.contains(status.overall_status))
      JSON_Object(Document_Status.Overall_Status.values.flatMap(s =>
        by_status.get(s).map(thys_key(s) -> _))) ++
      JSON_Object.if_proper(shown.nonEmpty,
        JSON_Object("thys" -> (show match {
          case Show.names => shown.map(print_status_node_name)
          case _ => JSON_Object(shown.map(status =>
            print_status_node_name(status) -> status.json(resources.unicode_symbols, cwd)))
        })))
    }
  }
}

sealed case class PIDE_AI_Theory_Status(
  node_name: Document.Node.Name,
  overall_status: Document_Status.Overall_Status,
  timing_ms: Long,
  percentage: Int,
  header_errors: List[PIDE_AI_Message],
  counts: Map[PIDE_AI_Command_Status, Int]
) {
  def is_ok: Boolean = overall_status == Document_Status.Overall_Status.ok
  def is_failed: Boolean = overall_status == Document_Status.Overall_Status.failed
  def is_pending: Boolean = overall_status == Document_Status.Overall_Status.pending

  def json(unicode_symbols: Boolean, cwd: Option[Path]): JSON.Object.T =
    JSON_Object.flatten(
      Some("status" -> overall_status.toString),
      Some("timing_ms" -> timing_ms),
      Option.when(percentage < 100)("percent_processed" -> percentage)) ++
    JSON.optional("header_errors" ->
      proper_list(header_errors.map(_.text(unicode_symbols, cwd)))) ++
    JSON_Object(PIDE_AI_Command_Status.values.toList
      .flatMap(status => counts.get(status).map(
        PIDE_AI_Document_Command.States.cmds_key(status.name) -> _)))
}
