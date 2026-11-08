/*  Title:      AI/src/PIDE/pide_ai_theory.scala
    Author:     Kevin Kappelmann

Theory utilities.
*/

package isabelle.pide.ai

import isabelle._

import scala.collection.mutable

object PIDE_AI_Theory {
  object Status {
    // cf. Document_Status.Node_Status.make, Document_Status.Nodes_Status.overall_status
    def apply(snapshot: Document.Snapshot, name: Document.Node.Name): Status = {
      val node = snapshot.version.nodes(name)
      var theory_status = Document_Status.Theory_Status.NONE
      var total = 0
      val counts = mutable.Map[PIDE_AI_Document_Command.Status, Int]().withDefaultValue(0)
      for (cmd <- node.commands.iterator) {
        val status = snapshot.state.command_status(snapshot.version, cmd)
        theory_status = Document_Status.Theory_Status.merge(theory_status, status.theory_status)
        if (!PIDE_AI_Document_Command.is_ignorable(snapshot, cmd)) {
          total += 1
          PIDE_AI_Document_Command.statuses(status).foreach(counts(_) += 1)
        }
      }

      def percent(a: Int, b: Int): Int =
        if (b == 0) 0 else ((a.toDouble / b) * 100).toInt

      val consolidated = Document_Status.Theory_Status.consolidated(theory_status)
      val percentage =
        node.get_theory match {
          case None =>
            if (consolidated) 100
            else percent(total - counts(PIDE_AI_Document_Command.Status.unprocessed), total).min(99)
          case Some(command) =>
            val processed =
              snapshot.state.command_status(snapshot.version, command).timings.count_finished
            percent(processed, command.span.theory_commands)
        }
      val overall_status =
        if (!consolidated) Document_Status.Overall_Status.pending
        else if (counts(PIDE_AI_Document_Command.Status.failed) == 0)
          Document_Status.Overall_Status.ok
        else Document_Status.Overall_Status.failed
      Status(name, overall_status, percentage, counts.toMap)
    }

    def statuses(snapshot: Document.Snapshot, names: List[Document.Node.Name]): List[Status] =
      names.map(apply(snapshot, _))

    val status_keys = Document_Status.Overall_Status.values.map(_.toString).toSet
    def prefix_keys(obj: JSON.Object.T): JSON.Object.T =
      JSON_Object.prefix_keys("thys_", status_keys)(obj)

    def statuses_json(
      resources: Resources,
      statuses: List[Status],
      cwd: Option[Path]
    ): JSON.Object.T = {
      val by_status = statuses.groupMapReduce(_.overall_status)(_ => 1)(_ + _)
      val theory_status_counts = JSON_Object(Document_Status.Overall_Status.values.flatMap(s =>
        by_status.get(s).map(s.toString -> _)))
      prefix_keys(theory_status_counts) ++
      JSON_Object.if_proper(statuses.nonEmpty,
        JSON_Object("thys" -> JSON_Object(statuses.map(status =>
          PIDE_AI_Origin.print(resources, status.node_name, cwd) -> status.json))))
    }
  }

  sealed case class Status(
    node_name: Document.Node.Name,
    overall_status: Document_Status.Overall_Status,
    percentage: Int,
    counts: Map[PIDE_AI_Document_Command.Status, Int]
  ) {
    def is_ok: Boolean = overall_status == Document_Status.Overall_Status.ok
    def is_failed: Boolean = overall_status == Document_Status.Overall_Status.failed
    def is_pending: Boolean = overall_status == Document_Status.Overall_Status.pending

    def json: JSON.Object.T = {
      val command_status_counts = JSON_Object(
        PIDE_AI_Document_Command.Status.values.toList
          .filterNot(status => is_ok && status == PIDE_AI_Document_Command.Status.finished)
          .flatMap(status => counts.get(status).map(status.name -> _)))
      JSON_Object("status" -> overall_status.toString) ++
      JSON.optional("percent_processed" -> Option.when(percentage < 100)(percentage)) ++
      PIDE_AI_Document_Command.Status.prefix_keys(command_status_counts)
    }
  }
}
