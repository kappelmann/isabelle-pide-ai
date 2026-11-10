/*  Title:      Symbols/src/symbols.scala
    Author:     Kevin Kappelmann

Encode or decode Isabelle symbols.
*/

package isabelle.symbols

import isabelle._

object Symbols {
  // cf. Isabelle/VSCode extension languages
  def supports_isabelle_symbols(path: Path): Boolean = {
    val name = path.file_name
    File.is_thy(name) || File.is_ML(name) || name.endsWith(".sml") || name.endsWith(".sig")
  }

  def output_decoded(unicode_symbols: Boolean, text: String): String =
    if (unicode_symbols) text else Symbol.encode(text)

  def recode_file(unicode_symbols: Boolean, path: Path): Boolean = {
    val text = File.read(path)
    val text1 = Symbol.output(unicode_symbols, text)
    val changed = text1 != text
    if (changed) File.write(path, text1)
    changed
  }

  def recode_sessions(
    unicode_symbols: Boolean,
    sessions_structure: Sessions.Structure,
    sessions: List[String],
    requirements: Boolean,
    base_logics: List[String]
  ): List[Path] = {
    val exclude =
      sessions_structure.selection(Sessions.Selection(sessions = base_logics)).build_graph.domain
    for {
      (name, base) <- Sessions.deps(sessions_structure.selection(Sessions.Selection(
        sessions = sessions)), inlined_files = true).session_bases.toList
      if (requirements || sessions.contains(name)) && !exclude(name)
      (path, _) <- base.session_sources
      if supports_isabelle_symbols(path) && recode_file(unicode_symbols, path)
    } yield path
  }

  val isabelle_tool = Isabelle_Tool("symbols", "encode or decode Isabelle symbols",
    Scala_Project.here,
    { args =>
      var requirements = false
      var unicode_symbols = false
      var dirs: List[Path] = Nil
      var base_logics = List(Isabelle_System.default_logic())
      var sessions: List[String] = Nil

      val getopts = Getopts("""
Usage: isabelle symbols [OPTIONS] [FILES ...]

  Options are:
    -R           also recode requirements of selected sessions
    -U           recode to Unicode symbols (default: ASCII)
    -d DIR       include session directory
    -l NAMES     comma-separated list of base logics, to remain unchanged
                 (default: """ + quote(Isabelle_System.default_logic()) + """)
    -s NAME      recode session NAME

  Recode Isabelle symbols in the given files and sessions.
""",
        "R" -> (_ => requirements = true),
        "U" -> (_ => unicode_symbols = true),
        "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
        "l:" -> (arg => base_logics = space_explode(',', arg)),
        "s:" -> (arg => sessions = sessions ::: List(arg)))

      val files = getopts(args)
      if (files.isEmpty && sessions.isEmpty) getopts.usage()

      val progress = new Console_Progress()
      val recoded =
        files.map(Path.explode).filter { path =>
          if (supports_isabelle_symbols(path)) recode_file(unicode_symbols, path)
          else {
            progress.echo_warning("Skipped file not supporting Isabelle symbols " + path)
            false
          }
        } :::
        (if (sessions.isEmpty) Nil
         else {
           recode_sessions(unicode_symbols, Sessions.load_structure(Options.init(), dirs = dirs),
             sessions, requirements = requirements, base_logics = base_logics)
         })
      for (path <- recoded) progress.echo("Recoded " + path)
    })
}

class Tools extends Isabelle_Scala_Tools(Symbols.isabelle_tool)
