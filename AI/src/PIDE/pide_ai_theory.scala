/*  Title:      AI/src/PIDE/pide_ai_theory.scala
    Author:     Kevin Kappelmann

Theory utilities.
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Theory {
  object Loaded {
    def apply(session: PIDE_AI_Session, nodes: Document.Nodes): Loaded = {
      val all_nodes = nodes.topological_order.filter(_.is_theory)
      val (base_session, dynamic) = all_nodes.partition(session.is_base_session_theory)
      Loaded(base_session, dynamic.filter(name => PIDE_AI_Util.is_loaded_dynamic(nodes(name))))
    }
  }

  sealed case class Loaded private(
    base_session: List[Document.Node.Name],
    dynamic: List[Document.Node.Name])

  object Status {
    def statuses(
      session: PIDE_AI_Session,
      snapshot: Document.Snapshot,
      names: List[Document.Node.Name]
    ): List[Status] = {
      val nodes_status = Document_Status.Nodes_Status.empty.update_nodes(
        Date.now(), session.resources, snapshot.state, snapshot.version,
        domain = Some(names.toSet))
      names.map(name => Status(name, nodes_status(name), nodes_status.overall_status(name)))
    }

    val status_keys = Document_Status.Overall_Status.values.map(_.toString).toSet
    def prefix_keys(obj: JSON.Object.T): JSON.Object.T =
      JSON_Object.prefix_keys("thys_", status_keys)(obj)

    def statuses_json(
      session: PIDE_AI_Session,
      statuses: List[Status],
      cwd: Option[Path]
    ): JSON.Object.T = {
      val by_status = statuses.groupMapReduce(_.overall_status)(_ => 1)(_ + _)
      val theory_status_counts = JSON_Object(Document_Status.Overall_Status.values.flatMap(s =>
        by_status.get(s).map(s.toString -> _)))
      prefix_keys(theory_status_counts) ++
      JSON_Object.if_proper(statuses.nonEmpty,
        JSON_Object("thys" -> JSON_Object(statuses.map(status =>
          session.print_origin(status.node_name, cwd) -> status.json))))
    }
  }

  sealed case class Status(
    node_name: Document.Node.Name,
    node_status: Document_Status.Node_Status,
    overall_status: Document_Status.Overall_Status
  ) {
    def json: JSON.Object.T = {
      val ok = overall_status == Document_Status.Overall_Status.ok
      val command_status_counts = JSON_Object((List(
        PIDE_AI_Document_Command.Status.unprocessed.name -> node_status.unprocessed,
        PIDE_AI_Document_Command.Status.running.name -> node_status.running,
        PIDE_AI_Document_Command.Status.failed.name -> node_status.failed,
        PIDE_AI_Document_Command.Status.warned.name -> node_status.warned) :::
        Option.when(!ok)(PIDE_AI_Document_Command.Status.finished.name -> node_status.finished).toList)
        .filter(_._2 > 0))
      JSON_Object("status" -> overall_status.toString) ++
      JSON.optional("percent_processed" ->
        Option.when(node_status.percentage < 100)(node_status.percentage)) ++
      PIDE_AI_Document_Command.Status.prefix_keys(command_status_counts)
    }
  }

  def commands_running_json(
    session: PIDE_AI_Session,
    snapshot: Document.Snapshot,
    names: List[Document.Node.Name],
    cwd: Option[Path]
  ): JSON.Object.T =
    JSON_Object(names.flatMap { name =>
      val node_snapshot = session.switch(snapshot, name)
      lazy val doc = Line.Document(node_snapshot.node.source)
      val entries = (for {
        state <- PIDE_AI_Document_Command.States(node_snapshot, None, session.unicode_symbols)
        if state.status.contains(PIDE_AI_Document_Command.Status.running)
      } yield state.problem(PIDE_AI_Document_Command.Status.running).json(doc, 1, cwd)).toList
      proper_list(entries).map(session.print_origin(name, cwd) -> _)
    })
}
