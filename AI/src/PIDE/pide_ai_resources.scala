/*  Title:      AI/src/PIDE/pide_ai_resources.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._
import isabelle.symbols.Symbols

import scala.annotation.tailrec

object PIDE_AI_Resources {
  def unicode_symbols(options: Options): Boolean = options.bool("pide_ai_unicode_symbols")
  def unicode_symbols_disk(options: Options): Boolean =
    options.bool("pide_ai_unicode_symbols_disk")
}

class PIDE_AI_Resources(
  val options: Options,
  session_background: Sessions.Background,
  log: Logger
) extends Resources(session_background, session_background, log) {
  def unicode_symbols: Boolean = PIDE_AI_Resources.unicode_symbols(options)
  def unicode_symbols_disk: Boolean = PIDE_AI_Resources.unicode_symbols_disk(options)

  override def append_path(prefix: String, source_path: Path): String =
    File.symbolic_path((Path.explode(prefix) + source_path).canonical)

  def is_base_session_theory(name: Document.Node.Name): Boolean = loaded_theory(name)

  def theory_node_name(theory: String): Option[Document.Node.Name] =
    if (Url.is_base_name(theory)) find_theory_node(theory) else None

  def file_node_name(path: Path): Option[Document.Node.Name] =
    find_theory(path.file).orElse { // session theories
      val node = append_path("", path)
      val candidate = Document.Node.Name(node, theory = Thy_Header.theory_name(node))
      Option.when(candidate.path.is_file)(candidate)
    }

  def is_loadable(name: Document.Node.Name): Boolean =
    name.path.is_file || make_theory_content(name).isDefined

  def unresolved_node(s: String): Nothing =
    error(s"${quote(s)} is neither a theory nor an existing file")

  def node_name(s: String, cwd: Option[Path]): Document.Node.Name =
    theory_node_name(s).orElse(file_node_name(PIDE_AI_Util.path(s, cwd)))
      .getOrElse(unresolved_node(s))

  def source_node_name(store: Store, source_name: String): Document.Node.Name =
    store.source_file(source_name).flatMap(PIDE_AI_Util.absolute_file)
      .flatMap(file_node_name).getOrElse(unresolved_node(source_name))

  def pide_text(path: Path, text: String): String =
    if (Symbols.supports_isabelle_symbols(path)) Symbol.output(unicode_symbols, text) else text

  def disk_text(path: Path, text: String): String =
    if (Symbols.supports_isabelle_symbols(path)) Symbol.output(unicode_symbols_disk, text)
    else text

  def read_file_content(node_name: Document.Node.Name): String =
    make_theory_content(node_name).getOrElse(
      pide_text(node_name.path, Line.normalize(File.read(node_name.path))))

  def write_file_content(path: Path, text: String): Unit =
    File.write(path, disk_text(path, Line.normalize(text)))

  def print_node_name(node_name: Document.Node.Name, cwd: Option[Path]): String =
    // roundtrip-property
    if (theory_node_name(node_name.theory).contains(node_name))
      node_name.theory // session-qualified
    else PIDE_AI_Util.print_file(node_name.node, cwd)


  /* dependencies */

  def known_dependency_closure(
    nodes: Document.Nodes,
    names: List[Document.Node.Name]
  ): List[Document.Node.Name] = {
    @tailrec def collect(
      seen: Set[Document.Node.Name],
      todo: List[Document.Node.Name]
    ): Set[Document.Node.Name] =
      todo match {
        case Nil => seen
        case name :: rest if seen(name) => collect(seen, rest)
        case name :: rest =>
          val node = nodes(name)
          collect(seen + name, node.thy.imports_no_pos ::: PIDE_AI_Nodes.blobs_names(node) :::
            make_theory_name(name).toList ::: rest)
      }
    collect(Set.empty, names).toList
  }

  def disk_dependency_closure(
    conditions: Thy_Conditions.Context,
    names: List[Document.Node.Name],
    progress: PIDE_AI_Progress
  ): List[Document.Node.Name] = {
    val deps = dependencies(conditions,
      names.filter(_.is_theory).map((_, Position.none)), progress = progress)
    val blobs = try deps.loaded_files catch { case ERROR(_) => Nil }
    (names ::: deps.theories ::: blobs).distinct
  }

  def nodes_to_load(
    conditions: Thy_Conditions.Context,
    nodes: Document.Nodes,
    node_names: List[Document.Node.Name],
    exclude: Set[Document.Node.Name],
    progress: PIDE_AI_Progress
  ): List[Document.Node.Name] = {
    def is_unloaded(name: Document.Node.Name): Boolean =
      !exclude(name) && (!is_base_session_theory(name) || node_names.contains(name)) &&
      !PIDE_AI_Nodes.is_loaded(nodes, name) && is_loadable(name)
    val unloaded = known_dependency_closure(nodes, node_names).filter(is_unloaded)
    disk_dependency_closure(conditions, unloaded, progress).filter(is_unloaded)
  }

  def known_descendant_closure(
    nodes: Document.Nodes,
    names: List[Document.Node.Name]
  ): List[Document.Node.Name] = {
    val loaded_theories = current_background.base.loaded_theories
    val import_names =
      loaded_theories.all_succs(names.filter(is_base_session_theory).map(_.theory))
        .map(Document.Node.Name.loaded_theory)
    nodes.descendants(names ::: import_names)
  }

  def loaded_descendant_closure(
    nodes: Document.Nodes,
    names: List[Document.Node.Name]
  ): List[Document.Node.Name] =
    known_descendant_closure(nodes, names).filter(PIDE_AI_Nodes.is_loaded(nodes, _))

  def orphaned_blobs(
    nodes: Document.Nodes,
    removed: List[Document.Node.Name]
  ): List[Document.Node.Name] = {
    def is_loaded(name: Document.Node.Name) = PIDE_AI_Nodes.is_loaded(nodes, name)
    val blobs = removed.flatMap(name => PIDE_AI_Nodes.blobs_names(nodes(name))).distinct
      .filter(is_loaded)
      .map(blob_name => blob_name -> make_theory_name(blob_name).filter(is_loaded))
    val removed1 = removed.toSet ++ blobs.flatMap(_._2)
    for {
      (blob_name, opt_thy) <- blobs
      if nodes.commands_loading(blob_name).forall(cmd => removed1(cmd.node_name))
      name <- blob_name :: opt_thy.toList if !removed.contains(name)
    } yield name
  }

  def nodes_to_unload(
    nodes: Document.Nodes,
    names: List[Document.Node.Name]
  ): List[Document.Node.Name] = {
    val descendants =
      loaded_descendant_closure(nodes, names ::: names.flatMap(make_theory_name))
    descendants ::: orphaned_blobs(nodes, descendants)
  }
}
