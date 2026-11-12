/*  Title:      AI/src/PIDE/pide_ai_nodes.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Nodes {
  def blobs_names(node: Document.Node): List[Document.Node.Name] =
    node.load_commands.flatMap(_.blobs_names)

  def loading_command(nodes: Document.Nodes, name: Document.Node.Name): Option[Command] =
    nodes.commands_loading(name).headOption

  def node_command(nodes: Document.Nodes, name: Document.Node.Name): Option[Command] =
    if (name.is_theory) nodes(name).get_theory else loading_command(nodes, name)

  def node_theory(nodes: Document.Nodes, name: Document.Node.Name): Option[Document.Node.Name] =
    if (name.is_theory) Some(name) else loading_command(nodes, name).map(_.node_name)

  def header_command(node: Document.Node): Option[Command] =
    node.commands.iterator.find(_.span.name == Thy_Header.THEORY)

  def failed_header_state(
    node_snapshot: Document.Snapshot
  ): Option[PIDE_AI_Document_Command.State] =
    header_command(node_snapshot.node)
      .filter(PIDE_AI_Command_Status.command_status(node_snapshot, _).is_failed)
      .flatMap(cmd => PIDE_AI_Document_Command.States(node_snapshot,
        PIDE_AI_Document_Command.Selection.commands(List(cmd -> cmd.range))).nextOption())

  def blocked_by(
    resources: PIDE_AI_Resources,
    node_snapshot: Document.Snapshot
  ): List[(Document.Node.Name, PIDE_AI_Document_Command.State)] =
    for {
      thy <- node_theory(node_snapshot.version.nodes, node_snapshot.node_name).toList
      dep <- resources.known_dependency_closure(node_snapshot.version.nodes, List(thy))
      header <- failed_header_state(node_snapshot.switch(dep))
    } yield dep -> header

  object Loaded {
    def apply(resources: PIDE_AI_Resources, nodes: Document.Nodes): Loaded = {
      val ordered = nodes.topological_order
      val unordered = nodes.names_iterator.filterNot(ordered.toSet).toList
      val all_nodes = (ordered ::: unordered).filter(_.is_theory)
      val (base_session, dynamic) = all_nodes.partition(resources.is_base_session_thy)
      Loaded(base_session.distinctBy(_.theory),
        dynamic.filter(resources.is_loaded_dynamic_thy(nodes, _)))
    }
  }

  sealed case class Loaded private(
    base_session: List[Document.Node.Name],
    dynamic: List[Document.Node.Name])
}
