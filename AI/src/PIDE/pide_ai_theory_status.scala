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
    val counts = mutable.Map[PIDE_AI_Command_Status, Int]().withDefaultValue(0)
    for (cmd <- node.commands.iterator) {
      val status = snapshot.state.command_status(snapshot.version, cmd)
      theory_status = Document_Status.Theory_Status.merge(theory_status, status.theory_status)
      if (!PIDE_AI_Document_Command.is_ignorable(snapshot, cmd)) {
        total += 1
        PIDE_AI_Command_Status.statuses(status).foreach(counts(_) += 1)
      }
    }

    def percent(a: Int, b: Int): Int =
      if (b == 0) 0 else ((a.toDouble / b) * 100).toInt

    val consolidated = Document_Status.Theory_Status.consolidated(theory_status)
    val percentage =
      node.get_theory match {
        case None =>
          if (consolidated) 100
          else percent(total - counts(PIDE_AI_Command_Status.unprocessed), total).min(99)
        case Some(command) =>
          val processed =
            snapshot.state.command_status(snapshot.version, command).timings.count_finished
          percent(processed, command.span.theory_commands)
      }
    val overall_status =
      if (!consolidated) Document_Status.Overall_Status.pending
      else if (counts(PIDE_AI_Command_Status.failed) == 0)
        Document_Status.Overall_Status.ok
      else Document_Status.Overall_Status.failed
    val header_errors = PIDE_AI_Nodes.header_state(snapshot.switch(name))
      .filter(_.command_status.is_failed).toList.flatMap(_.errors)
    PIDE_AI_Theory_Status(name, overall_status, percentage, counts.toMap, header_errors)
  }

  object Statuses {
    def apply(
      snapshot: Document.Snapshot,
      names: List[Document.Node.Name]
    ): List[PIDE_AI_Theory_Status] =
      names.map(PIDE_AI_Theory_Status(snapshot, _))

    def thys_key(overall_status: Document_Status.Overall_Status): String = "thys_" + overall_status

    def json(
      resources: Resources,
      statuses: List[PIDE_AI_Theory_Status],
      unicode_symbols: Boolean,
      cwd: Option[Path]
    ): JSON.Object.T = {
      val by_status = statuses.groupMapReduce(_.overall_status)(_ => 1)(_ + _)
      JSON_Object(Document_Status.Overall_Status.values.flatMap(s =>
        by_status.get(s).map(thys_key(s) -> _))) ++
      JSON_Object.if_proper(statuses.nonEmpty,
        JSON_Object("thys" -> JSON_Object(statuses.map(status =>
          PIDE_AI_Origin.print(resources, status.node_name, cwd) ->
            status.json(unicode_symbols, cwd)))))
    }
  }
}

sealed case class PIDE_AI_Theory_Status(
  node_name: Document.Node.Name,
  overall_status: Document_Status.Overall_Status,
  percentage: Int,
  counts: Map[PIDE_AI_Command_Status, Int],
  header_errors: List[PIDE_AI_Message]
) {
  def is_ok: Boolean = overall_status == Document_Status.Overall_Status.ok
  def is_failed: Boolean = overall_status == Document_Status.Overall_Status.failed
  def is_pending: Boolean = overall_status == Document_Status.Overall_Status.pending

  def json(unicode_symbols: Boolean, cwd: Option[Path]): JSON.Object.T =
    JSON_Object("status" -> overall_status.toString) ++
    JSON.optional("percent_processed" -> Option.when(percentage < 100)(percentage)) ++
    JSON_Object(PIDE_AI_Command_Status.values.toList
      .filterNot(status => is_ok && status == PIDE_AI_Command_Status.finished)
      .flatMap(status => counts.get(status).map(
        PIDE_AI_Document_Command.States.cmds_key(status.name) -> _))) ++
    JSON.optional("header_errors" ->
      proper_list(header_errors.map(_.text(unicode_symbols, cwd))))
}
