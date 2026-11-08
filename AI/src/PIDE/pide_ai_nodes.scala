/*  Title:      AI/src/PIDE/pide_ai_nodes.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Nodes {
  def is_base_session_theory(resources: Resources, name: Document.Node.Name): Boolean =
    resources.loaded_theory(name)

  // a cleaned node remains in version.nodes but with empty source
  def is_loaded_dynamic(node: Document.Node): Boolean =
    node.get_blob match {
      case Some(blob) => blob.source.nonEmpty
      case None => !node.is_empty
    }

  def is_loadable(resources: Resources, name: Document.Node.Name): Boolean =
    name.path.is_file || resources.make_theory_content(name).isDefined

  def loading_command(nodes: Document.Nodes, name: Document.Node.Name): Option[Command] =
    nodes.commands_loading(name).headOption

  def node_command(nodes: Document.Nodes, name: Document.Node.Name): Option[Command] =
    if (name.is_theory) nodes(name).get_theory else loading_command(nodes, name)

  def node_theory(nodes: Document.Nodes, name: Document.Node.Name): Option[Document.Node.Name] =
    if (name.is_theory) Some(name) else loading_command(nodes, name).map(_.node_name)

  object Loaded {
    def apply(resources: Resources, nodes: Document.Nodes): Loaded = {
      val all_nodes = nodes.topological_order.filter(_.is_theory)
      val (base_session, dynamic) = all_nodes.partition(is_base_session_theory(resources, _))
      Loaded(base_session, dynamic.filter(name => is_loaded_dynamic(nodes(name))))
    }
  }

  sealed case class Loaded private(
    base_session: List[Document.Node.Name],
    dynamic: List[Document.Node.Name])
}
