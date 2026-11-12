/*  Title:      AI/src/Tools/pide_ai_tool_util.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Tool_Util {
  val retry_soon_message: String = "Please retry soon."

  def running_session_param(
    sessions: PIDE_AI_Sessions,
    args: JSON.Object.T
  ): PIDE_AI_Session = {
    def available = s"Available session(s): ${JSON.Format(sessions.state_json())}"
    val session_ids = PIDE_AI_Tool_Schema.running_session_arg.get(args).map(List(_))
    sessions.running_sessions(session_ids) match {
      case List(session) => session
      case Nil => error(s"No running session. $available")
      case _ => error(s"Multiple running sessions: specify session id. $available")
    }
  }

  def running_sessions_param(
    sessions: PIDE_AI_Sessions,
    args: JSON.Object.T
  ): List[PIDE_AI_Session] =
    sessions.running_sessions(PIDE_AI_Tool_Schema.running_sessions_arg.get(args))

  def node_param(
    session: PIDE_AI_Session,
    args: JSON.Object.T,
    cwd: Option[Path]
  ): Document.Node.Name =
    session.resources.node_name(session.snapshot().version.nodes,
      PIDE_AI_Tool_Schema.node_arg.get(args), cwd)

  enum Sync { case none, node, node_and_ancestors }

  enum Stop_On {
    case any_failed, all_terminated
    def stop(statuses: List[Document_Status.Command_Status]): Boolean =
      statuses.forall(_.is_terminated) || this == any_failed && statuses.exists(_.is_failed)
    def stop_thys(statuses: List[PIDE_AI_Theory_Status]): Boolean =
      statuses.forall(!_.is_pending) ||
        this == any_failed && statuses.exists(_.count(PIDE_AI_Command_Status.failed) > 0)
  }

  def sync(
    session: PIDE_AI_Session,
    node_names: List[Document.Node.Name],
    sync: Sync,
    progress: PIDE_AI_Progress
  ): Unit =
    sync match {
      case Sync.none =>
      case Sync.node =>
        session.resources.sync_nodes(session, node_names.map(_ -> None), hide_others = false,
          progress)
      case Sync.node_and_ancestors =>
        session.resources.sync(session, node_names.map(_ -> None), hide_others = false, progress)
    }

  def sync_load_missing_nodes(
    session: PIDE_AI_Session,
    nodes: List[(Document.Node.Name, Option[List[(Int, Option[Int])]])],
    sync: Sync,
    hide_others: Boolean,
    progress: PIDE_AI_Progress
  ): (Map[Document.Node.Name, String], Document.Change) =
    sync match {
      case Sync.none =>
        session.resources.load_missing_nodes(session, nodes, hide_others, progress)
      case Sync.node => session.resources.load_nodes(session, nodes, hide_others, progress)
      case Sync.node_and_ancestors =>
        session.with_lock(progress) {
          PIDE_AI_Tool_Util.sync(session, nodes.map(_._1), sync, progress)
          session.resources.load_missing_nodes(session, nodes, hide_others, progress)
        }
    }

  def sync_load_missing(
    session: PIDE_AI_Session,
    node_names: List[Document.Node.Name],
    sync: Sync,
    progress: PIDE_AI_Progress
  ): Document.Change =
    session.with_lock(progress) {
      PIDE_AI_Tool_Util.sync(session, node_names, sync, progress)
      session.resources.load_missing(session, node_names, progress)
    }

  def sync_load_missing_stable_node_snapshot(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    sync: Sync,
    progress: PIDE_AI_Progress,
    cwd: Option[Path]
  ): Document.Snapshot = {
    session.await_stable(sync_load_missing(session, List(node_name), sync, progress), progress)
    val snapshot = session.node_snapshot(node_name)
    if (PIDE_AI_Nodes.node_theory(snapshot.version.nodes, node_name).isEmpty)
      error("File " + quote(session.resources.print_node_name(node_name, cwd)) +
        " is not loaded by any theory. Load the containing theory first.")
    snapshot
  }
}
