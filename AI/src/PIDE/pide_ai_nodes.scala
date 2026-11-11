/*  Title:      AI/src/PIDE/pide_ai_nodes.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Nodes {
  // a cleaned node remains in version.nodes but with empty source
  def is_loaded_dynamic(nodes: Document.Nodes, name: Document.Node.Name): Boolean = {
    val node = nodes(name)
    node.get_blob match {
      case Some(blob) => blob.source.nonEmpty || nodes.commands_loading(name).nonEmpty
      case None => !node.is_empty
    }
  }

  def loading_command(nodes: Document.Nodes, name: Document.Node.Name): Option[Command] =
    nodes.commands_loading(name).headOption

  def node_command(nodes: Document.Nodes, name: Document.Node.Name): Option[Command] =
    if (name.is_theory) nodes(name).get_theory else loading_command(nodes, name)

  def node_theory(nodes: Document.Nodes, name: Document.Node.Name): Option[Document.Node.Name] =
    if (name.is_theory) Some(name) else loading_command(nodes, name).map(_.node_name)

  def header_command(node: Document.Node): Option[Command] =
    node.commands.iterator.find(_.span.name == Thy_Header.THEORY)

  def header_state(node_snapshot: Document.Snapshot): Option[PIDE_AI_Document_Command.State] =
    header_command(node_snapshot.node).flatMap(cmd =>
      PIDE_AI_Document_Command.States(node_snapshot,
        PIDE_AI_Document_Command.Selection.commands(List(cmd -> cmd.range))).nextOption())

  def blocked_by(
    resources: Resources,
    node_snapshot: Document.Snapshot
  ): List[(Document.Node.Name, PIDE_AI_Document_Command.State)] =
    for {
      thy <- node_theory(node_snapshot.version.nodes, node_snapshot.node_name).toList
      dep <- PIDE_AI_Dependencies.known_dependencies(resources, node_snapshot.version.nodes,
        List(thy))
      header <- header_state(node_snapshot.switch(dep))
      if header.command_status.is_failed
    } yield dep -> header

  object Loaded {
    def apply(resources: Resources, nodes: Document.Nodes): Loaded = {
      val ordered = nodes.topological_order
      val unordered = nodes.names_iterator.filterNot(ordered.toSet).toList
      val all_nodes = (ordered ::: unordered).filter(_.is_theory)
      val (base_session, dynamic) = all_nodes.partition(resources.loaded_theory)
      Loaded(base_session, dynamic.filter(is_loaded_dynamic(nodes, _)))
    }
  }

  sealed case class Loaded private(
    base_session: List[Document.Node.Name],
    dynamic: List[Document.Node.Name])
}
