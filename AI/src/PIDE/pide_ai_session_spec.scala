/*  Title:      AI/src/PIDE/pide_ai_session_spec.scala
    Author:     Kevin Kappelmann

Specifications of PIDE sessions.
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Session_Spec {
  def default_logic(): String = Isabelle_System.default_logic()

  def option(s: String): Options.Spec = Options.Spec.make(s)

  private val default_spec: PIDE_AI_Session_Spec = PIDE_AI_Session_Spec()

  val usage: String ="""Session options are:

    -A NAME      ancestor session for option -R (default: parent)
    -R NAME      build image with requirements from other sessions
    -d DIR       include session directory
    -f           fresh build
    -l NAME      logic session name (default ISABELLE_LOGIC=""" + quote(default_logic()) + """)
    -n           no build of session image on startup
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
    -z ID        PIDE AI session id
"""
  private object Parsers extends Scan.Parsers {
    val blanks: Parser[String] = many(character(Symbol.is_ascii_blank))
    val word: Parser[String] = quoted("\"") ^^ (s => quoted_content("\"", s))
      | many1(sym => sym != "\"" && !character(Symbol.is_ascii_blank)(sym))
    val words: Parser[List[String]] = blanks ~> rep(word <~ blanks)
  }

  class Builder {
    private var current: Option[PIDE_AI_Session_Spec] = None
    private var session_option = false
    def spec: Option[PIDE_AI_Session_Spec] = current
    def has_session_option: Boolean = session_option
    private def update(f: PIDE_AI_Session_Spec => PIDE_AI_Session_Spec): Unit =
      current = Some(f(current.getOrElse(default_spec)))
    private def update_session(f: PIDE_AI_Session_Spec => PIDE_AI_Session_Spec): Unit =
      { session_option = true; update(f) }
    def option_specs(cwd: Option[Path]): List[(String, String => Unit)] =
      List(
        "A:" -> (arg => update_session(_.copy(session_ancestor = Some(arg)))),
        "R:" -> (arg => update_session(_.copy(logic = arg, session_requirements = true))),
        "d:" -> (arg => update_session(spec =>
          spec.copy(dirs = spec.dirs ::: List(PIDE_AI_Util.path(arg, cwd))))),
        "f" -> (_ => update_session(_.copy(fresh_build = true))),
        "l:" -> (arg => update_session(_.copy(logic = arg))),
        "n" -> (_ => update_session(_.copy(no_build = true))),
        "o:" -> (arg => update(spec => spec.copy(
          options = spec.options ::: List(option(arg))))),
        "z:" -> (arg => update_session(_.copy(id = Some(arg)))))
  }

  def parse(spec: String, cwd: Option[Path]): PIDE_AI_Session_Spec = {
    val words = Parsers.parseAll(Parsers.words, spec) match {
      case Parsers.Success(res, _) => res
      case bad => cat_error(bad.toString, usage)
    }
    val builder = new Builder
    val more_args = Getopts(usage, builder.option_specs(cwd): _*)(words, true)
    if (more_args.nonEmpty) cat_error("Bad session options: " + quote(spec), usage)
    builder.spec.getOrElse(default_spec)
  }

  val option_format: JSON_Schema.Format[Options.Spec] =
    JSON_Schema.Format.string.map(option, _.toString)

  val session_arg = JSON_Schema.Arg.optional(
    "session_id", s"Default: generated based on ${quote("logic")}.",
    JSON_Schema.Format.string)
  val logic_arg = JSON_Schema.Arg.default(
    "logic", "Session to work with.", JSON_Schema.Format.string, default_spec.logic)
  val dirs_arg = JSON_Schema.Arg.default(
    "dirs", "Included directories.",
    JSON_Schema.Format.list(JSON_Schema.Format.string), default_spec.dirs.map(_.implode))
  val options_arg = JSON_Schema.Arg.default(
    "options",
    s"System options to override, each as ${quote("NAME=VAL")} or ${quote("NAME")}.",
    JSON_Schema.Format.list(option_format), default_spec.options)
  val session_ancestor_arg = JSON_Schema.Arg.optional(
    "session_ancestor",
    s"Session that the ${quote("session_requirements")} base session extends. " +
    s"Default: parent of ${quote("logic")}.",
    JSON_Schema.Format.string)
  val session_requirements_arg = JSON_Schema.Arg.default(
    "session_requirements",
    s"Build the base session only from what ${quote("logic")} needs from other sessions, " +
    s"so its theories stay editable. Otherwise, ${quote("logic")} is the base session.",
    JSON_Schema.Format.bool, default_spec.session_requirements)
  val fresh_build_arg = JSON_Schema.Arg.default(
    "fresh_build", "Force rebuild of base session.", JSON_Schema.Format.bool,
    default_spec.fresh_build)
  val no_build_arg = JSON_Schema.Arg.default(
    "no_build", "Do not build base session on startup and fail if not prebuilt.",
    JSON_Schema.Format.bool, default_spec.no_build)

  val json_args: List[JSON_Schema.Arg[?]] =
    List(session_arg, logic_arg, dirs_arg, options_arg, session_ancestor_arg,
      session_requirements_arg, fresh_build_arg, no_build_arg)

  def from_json(args: JSON.Object.T, cwd: Option[Path]): PIDE_AI_Session_Spec =
    PIDE_AI_Session_Spec(
      logic = logic_arg.get(args),
      id = session_arg.get(args),
      dirs = dirs_arg.get(args).map(PIDE_AI_Util.path(_, cwd)),
      options = options_arg.get(args),
      session_ancestor = session_ancestor_arg.get(args),
      session_requirements = session_requirements_arg.get(args),
      fresh_build = fresh_build_arg.get(args),
      no_build = no_build_arg.get(args))
}

sealed case class PIDE_AI_Session_Spec(
  session_ancestor: Option[String] = None,
  session_requirements: Boolean = false,
  dirs: List[Path] = Nil,
  fresh_build: Boolean = false,
  no_build: Boolean = false,
  logic: String = PIDE_AI_Session_Spec.default_logic(),
  options: Options.Update = Nil,
  id: Option[String] = None
) {
  def json: JSON.Object.T =
    PIDE_AI_Session_Spec.session_arg(id) ++
    PIDE_AI_Session_Spec.logic_arg(logic) ++
    PIDE_AI_Session_Spec.dirs_arg(dirs.map(_.implode)) ++
    PIDE_AI_Session_Spec.options_arg(options) ++
    PIDE_AI_Session_Spec.session_ancestor_arg(session_ancestor) ++
    PIDE_AI_Session_Spec.session_requirements_arg(session_requirements) ++
    PIDE_AI_Session_Spec.fresh_build_arg(fresh_build) ++
    PIDE_AI_Session_Spec.no_build_arg(no_build)
}
