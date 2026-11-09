/*  Title:      AI/src/PIDE/pide_ai_editor.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Editor {
  sealed case class Context(node_name: Document.Node.Name, offset: Text.Offset)
}

class PIDE_AI_Editor(mcp_session: PIDE_AI_Session, progress: PIDE_AI_Progress) extends Editor {
  type Context = PIDE_AI_Editor.Context
  type Session = PIDE_AI_Headless.Session

  def session: PIDE_AI_Headless.Session = mcp_session.session
  def flush(): Unit = ()
  def invoke(): Unit = ()
  def revoke(): Unit = ()

  def get_models(): Iterable[Document.Model] = Nil

  def current_node(context: Context): Option[Document.Node.Name] = Some(context.node_name)
  def node_snapshot(name: Document.Node.Name): Document.Snapshot = mcp_session.node_snapshot(name)
  def current_node_snapshot(context: Context): Option[Document.Snapshot] =
    try { Some(node_snapshot(context.node_name)) } catch { case ERROR(_) => None }
  def current_command(context: Context, snapshot: Document.Snapshot): Option[Command] =
    snapshot.current_command(context.node_name, context.offset)

  def output_state(): Boolean = mcp_session.options.bool("editor_output_state")

  def node_overlays(name: Document.Node.Name): Document.Node.Overlays =
    mcp_session.tip_version(progress).nodes(name).perspective.overlays
  def insert_overlay(command: Command, fn: String, args: List[String]): Unit =
    mcp_session.insert_overlay(command, fn, args, progress)
  def remove_overlay(command: Command, fn: String, args: List[String]): Unit =
    mcp_session.remove_overlay(command, fn, args, new Unstoppable_Progress(progress))

  def hyperlink_command(
    snapshot: Document.Snapshot,
    id: Document_ID.Generic,
    offset: Symbol.Offset = 0,
    description: String = "",
    focus: Boolean = false
  ): Option[Hyperlink] = None

  def assert_dispatcher[A](body: => A): A = session.assert_dispatcher(body)
  def require_dispatcher[A](body: => A): A = session.require_dispatcher(body)
  def send_dispatcher(body: => Unit): Unit = session.send_dispatcher(body)
  def send_wait_dispatcher(body: => Unit): Unit = session.send_wait_dispatcher(body)
}
