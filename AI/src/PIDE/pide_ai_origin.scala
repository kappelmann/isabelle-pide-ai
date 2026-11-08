/*  Title:      AI/src/PIDE/pide_ai_origin.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Origin {
  def theory_node_name(resources: Resources, theory: String): Option[Document.Node.Name] =
    if (Url.is_base_name(theory)) resources.find_theory_node(theory) else None

  def file_node_name(resources: Resources, path: Path): Option[Document.Node.Name] =
    resources.find_theory(path.file).orElse { // session theories
      val node = resources.append_path("", path)
      val candidate = Document.Node.Name(node, theory = Thy_Header.theory_name(node))
      Option.when(candidate.path.is_file)(candidate)
    }

  def unresolved(s: String): Nothing =
    error(s"${quote(s)} is neither a theory nor an existing file")

  def node_name(resources: Resources, s: String, cwd: Option[Path]): Document.Node.Name =
    theory_node_name(resources, s)
      .orElse(file_node_name(resources, PIDE_AI_Util.path(s, cwd))).getOrElse(unresolved(s))

  def source_node_name(
    resources: Resources,
    store: Store,
    source_name: String
  ): Document.Node.Name =
    store.source_file(source_name).flatMap(PIDE_AI_Util.absolute_file)
      .flatMap(file_node_name(resources, _)).getOrElse(unresolved(source_name))

  def print(resources: Resources, node_name: Document.Node.Name, cwd: Option[Path]): String =
    // roundtrip-property
    if (theory_node_name(resources, node_name.theory).contains(node_name))
      node_name.theory // session-qualified
    else PIDE_AI_Util.print_file(node_name.node, cwd)
}
