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

  def stable_synced(
    session: PIDE_AI_Session,
    node_names: List[Document.Node.Name],
    sync: Sync,
    progress: PIDE_AI_Progress
  ): Unit = {
    val change = sync match {
      case Sync.none => session.resolve_dependencies(node_names, progress)
      case Sync.node => session.sync_resolve(node_names, progress)
      case Sync.node_and_ancestors =>
        session.with_lock(progress) {
          session.sync_resolve(session.resources.known_dependency_closure(
            session.tip_version(progress).nodes, node_names), progress)
        }
    }
    session.await_stable(change, progress)
  }

  def require_node_snapshot(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    progress: PIDE_AI_Progress,
    cwd: Option[Path]
  ): Document.Snapshot = {
    def node = quote(session.resources.print_node_name(node_name, cwd))
    val snapshot = try { session.node_snapshot(node_name) } catch {
      case ERROR(_) =>
        if (session.resources.is_loaded_node(session.tip_version(progress).nodes, node_name))
          error(s"The node $node is loaded but has not been processed yet. " +
            retry_soon_message)
        else {
          session.read_update_resolve(node_name, Nil,
            await_stable_before_resolve = false, hide_others = false, progress = progress)
          error(s"The node $node was not loaded and has now been queued for loading. " +
            retry_soon_message)
        }
    }
    if (PIDE_AI_Nodes.node_theory(snapshot.version.nodes, node_name).isEmpty)
      error(s"File $node is not loaded by any theory. Load the containing theory first.")
    snapshot
  }

  def require_stable_synced_node_snapshot(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    sync: Sync,
    progress: PIDE_AI_Progress,
    cwd: Option[Path]
  ): Document.Snapshot = {
    stable_synced(session, List(node_name), sync, progress)
    require_node_snapshot(session, node_name, progress, cwd)
  }
}
