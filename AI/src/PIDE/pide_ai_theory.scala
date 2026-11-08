/*  Title:      AI/src/PIDE/pide_ai_theory.scala
    Author:     Kevin Kappelmann

Theory utilities.
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Theory {
  object Status {
    def statuses(
      resources: Resources,
      snapshot: Document.Snapshot,
      names: List[Document.Node.Name]
    ): List[Status] = {
      val nodes_status = Document_Status.Nodes_Status.empty.update_nodes(
        Date.now(), resources, snapshot.state, snapshot.version,
        domain = Some(names.toSet))
      names.map(name => Status(name, nodes_status(name), nodes_status.overall_status(name)))
    }

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
    node_status: Document_Status.Node_Status,
    overall_status: Document_Status.Overall_Status
  ) {
    def is_ok: Boolean = overall_status == Document_Status.Overall_Status.ok
    def is_failed: Boolean = overall_status == Document_Status.Overall_Status.failed
    def is_pending: Boolean = overall_status == Document_Status.Overall_Status.pending

    def json: JSON.Object.T = {
      val command_status_counts = JSON_Object((List(
        PIDE_AI_Document_Command.Status.unprocessed.name -> node_status.unprocessed,
        PIDE_AI_Document_Command.Status.running.name -> node_status.running,
        PIDE_AI_Document_Command.Status.failed.name -> node_status.failed,
        PIDE_AI_Document_Command.Status.warned.name -> node_status.warned) :::
        Option.when(!is_ok)(
          PIDE_AI_Document_Command.Status.finished.name -> node_status.finished).toList)
        .filter(_._2 > 0))
      JSON_Object("status" -> overall_status.toString) ++
      JSON.optional("percent_processed" ->
        Option.when(node_status.percentage < 100)(node_status.percentage)) ++
      PIDE_AI_Document_Command.Status.prefix_keys(command_status_counts)
    }
  }
}
