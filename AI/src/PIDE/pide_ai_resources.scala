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

  def node_string(path: Path): String = File.symbolic_path(path.canonical)

  override def append_path(prefix: String, source_path: Path): String =
    node_string(Path.explode(prefix) + source_path)

  def is_base_session_thy(name: Document.Node.Name): Boolean = loaded_theory(name)

  val base_session_blobs: Synchronized[Map[String, Boolean]] = Synchronized(Map.empty)

  lazy val base_sessions: List[String] =
    session_background.sessions_structure.build_requirements(
      List(session_background.session_name))

  lazy val store: Store = Store(options)

  def is_base_session_blob(name: Document.Node.Name): Boolean =
    !name.is_theory && base_session_blobs.value.getOrElse(name.node, {
      val blob = base_sessions.exists(session => store.try_open_database(session)
        .exists(db => using(db)(store.read_sources(_, session, name = name.node).nonEmpty)))
      base_session_blobs.change(_ + (name.node -> blob))
      blob
    })

  def is_base_session_node(name: Document.Node.Name): Boolean =
    is_base_session_thy(name) || is_base_session_blob(name)

  // a cleaned node remains in version.nodes but with empty source
  def is_loaded_thy(nodes: Document.Nodes, name: Document.Node.Name): Boolean =
    name.is_theory && !nodes(name).is_empty

  def is_loaded_blob(nodes: Document.Nodes, name: Document.Node.Name): Boolean =
    nodes(name).get_blob.exists(blob =>
      blob.source.nonEmpty || nodes.commands_loading(name).nonEmpty)

  def is_loaded_node(nodes: Document.Nodes, name: Document.Node.Name): Boolean =
    is_loaded_thy(nodes, name) || is_loaded_blob(nodes, name)

  def is_loaded_base_session_thy(nodes: Document.Nodes, name: Document.Node.Name): Boolean =
    is_loaded_thy(nodes, name) && is_base_session_thy(name)

  def is_loaded_base_session_blob(nodes: Document.Nodes, name: Document.Node.Name): Boolean =
    is_loaded_blob(nodes, name) &&
      (PIDE_AI_Nodes.loading_command(nodes, name) match {
        case Some(command) => is_base_session_thy(command.node_name)
        case None => is_base_session_blob(name)
      })

  def is_loaded_base_session_node(nodes: Document.Nodes, name: Document.Node.Name): Boolean =
    is_loaded_base_session_thy(nodes, name) || is_loaded_base_session_blob(nodes, name)

  def is_loaded_dynamic_thy(nodes: Document.Nodes, name: Document.Node.Name): Boolean =
    is_loaded_thy(nodes, name) && !is_base_session_thy(name)

  def is_loaded_dynamic_blob(nodes: Document.Nodes, name: Document.Node.Name): Boolean =
    is_loaded_blob(nodes, name) && !is_loaded_base_session_blob(nodes, name)

  def is_loaded_dynamic_node(nodes: Document.Nodes, name: Document.Node.Name): Boolean =
    is_loaded_dynamic_thy(nodes, name) || is_loaded_dynamic_blob(nodes, name)

  def theory_node_name(theory: String): Option[Document.Node.Name] =
    if (Url.is_base_name(theory)) {
      val base_name = Long_Name.base_name(theory)
      find_theory_node(
        if (sessions_structure.global_theories.get(base_name).contains(Long_Name.qualifier(theory)))
          base_name
        else theory)
    }
    else None

  def file_node_name(nodes: Document.Nodes, path: Path): Option[Document.Node.Name] = {
    val node = node_string(path)
    nodes.names_iterator.find(name => name.node == node && is_loaded_node(nodes, name))
      .orElse(find_theory(path.file)) // session theories
      .orElse {
        val candidate = Document.Node.Name(node, theory = Thy_Header.theory_name(node))
        Option.when(candidate.path.is_file)(candidate)
      }
  }

  def is_loadable(name: Document.Node.Name): Boolean =
    name.path.is_file || make_theory_content(name).isDefined

  def unresolved_node(s: String): Nothing =
    error(s"${quote(s)} is neither a theory nor an existing file")

  def node_name(nodes: Document.Nodes, s: String, cwd: Option[Path]): Document.Node.Name =
    theory_node_name(s).orElse(file_node_name(nodes, PIDE_AI_Util.path(s, cwd)))
      .getOrElse(unresolved_node(s))

  def source_node_name(
    store: Store,
    nodes: Document.Nodes,
    source_name: String
  ): Document.Node.Name =
    store.source_file(source_name).flatMap(PIDE_AI_Util.absolute_file)
      .flatMap(file_node_name(nodes, _)).getOrElse(unresolved_node(source_name))

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
      !exclude(name) && (!is_base_session_thy(name) || node_names.contains(name)) &&
      !is_loaded_node(nodes, name) && is_loadable(name)
    val unloaded = known_dependency_closure(nodes, node_names).filter(is_unloaded)
    disk_dependency_closure(conditions, unloaded, progress).filter(is_unloaded)
  }

  def known_descendant_closure(
    nodes: Document.Nodes,
    names: List[Document.Node.Name]
  ): List[Document.Node.Name] = {
    val loaded_theories = current_background.base.loaded_theories
    val import_names =
      loaded_theories.all_succs(names.filter(is_base_session_thy).map(_.theory))
        .map(Document.Node.Name.loaded_theory)
    nodes.descendants(names ::: import_names)
  }

  def loaded_descendant_closure(
    nodes: Document.Nodes,
    names: List[Document.Node.Name]
  ): List[Document.Node.Name] =
    known_descendant_closure(nodes, names).filter(is_loaded_node(nodes, _))

  def orphaned_blobs(
    nodes: Document.Nodes,
    removed: List[Document.Node.Name]
  ): List[Document.Node.Name] = {
    val blobs = removed.flatMap(name => PIDE_AI_Nodes.blobs_names(nodes(name))).distinct
      .filter(is_loaded_node(nodes, _))
      .map(blob_name => blob_name -> make_theory_name(blob_name).filter(is_loaded_node(nodes, _)))
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
