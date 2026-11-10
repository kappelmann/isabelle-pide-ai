/*  Title:      AI/src/PIDE/pide_ai_resources.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._
import isabelle.symbols.Symbols

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
}
