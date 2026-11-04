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
      case _ => error(s"Multiple running sessions: specify ${quote("session")}. $available")
    }
  }

  def running_sessions_param(
    sessions: PIDE_AI_Sessions,
    args: JSON.Object.T
  ): List[PIDE_AI_Session] =
    sessions.running_sessions(PIDE_AI_Tool_Schema.running_sessions_arg.get(args))

  def origin_param(session: PIDE_AI_Session, args: JSON.Object.T): Document.Node.Name =
    session.node_name(PIDE_AI_Tool_Schema.origin_arg.get(args))

  def require_origin_snapshot(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    progress: Progress
  ): Document.Snapshot = {
    val snapshot = try { session.node_snapshot(node_name) } catch {
      case ERROR(_) =>
        if (PIDE_AI_Util.is_loaded_dynamic(session.tip_version(progress).nodes(node_name)))
          error(s"The origin ${quote(session.origin(node_name))} is loaded but has not been " +
            "processed yet. " + retry_soon_message)
        else {
          session.read_update_resolve(node_name, Nil,
            await_stable_before_resolve = false, hide_others = false, progress = progress)
          error(s"The origin ${quote(session.origin(node_name))} was not loaded and has now " +
            "been queued for loading. " + retry_soon_message)
        }
    }
    if (!node_name.is_theory && snapshot.commands_loading.isEmpty)
      error(s"File ${quote(session.origin(node_name))} is not loaded by any theory. " +
        "Load the containing theory first.")
    snapshot
  }

  def require_stable_origin_snapshot(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    progress: Progress
  ): Document.Snapshot = {
    session.await_stable(session.sync_dependencies_resolve(List(node_name), progress), progress)
    require_origin_snapshot(session, node_name, progress)
  }
}
