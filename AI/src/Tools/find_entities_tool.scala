/*  Title:      AI/src/Tools/find_entities_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object Find_Entities_Tool {
  enum Kind(val markups: List[String], val hint: String = "") {
    case facts extends Kind(
      List(Markup.FACT, Markup.DYNAMIC_FACT, Markup.LITERAL_FACT, Markup.AXIOM))
    case constant extends Kind(List(Markup.CONSTANT))
    case type_name extends Kind(List(Markup.TYPE_NAME))
    case `class` extends Kind(List(Markup.CLASS))
    case fixed extends Kind(List(Markup.FIXED))
    case ml extends Kind(List(Markup.ML_DEF, Markup.ML_ANTIQUOTATION))
    case modules extends Kind(List(Markup.THEORY, Markup.SESSION, Markup.LOCALE, Markup.BUNDLE),
      "theories, sessions, locales, bundles,...")
    case isar extends Kind(List(Markup.COMMAND, Markup.METHOD, Markup.ATTRIBUTE, Markup.CASE),
      "commands, methods, attributes,...")
    case presentation extends Kind(
      List(Markup.DOCUMENT_ANTIQUOTATION, Markup.DOCUMENT_ANTIQUOTATION_OPTION,
        "antiquotation_style", "document_marker"), "document antiquotations, markers,...")
    case misc extends Kind(
      List("simproc", Markup.ORACLE, "plugin", "antiquotation", "system_option", "tool"),
      "simprocs, system options,...")

    def name: String = if (markups.length == 1) markups.head else toString
  }
}

class Find_Entities_Tool extends PIDE_AI_Tool("find_entities") with PIDE_AI_Tool.Read_Only {
  def description: String =
    "Find definition locations for entities (theorems, commands,...). " +
    PIDE_AI_Tool_Schema.implicit_load_file

  val snippet_lines_arg = JSON_Schema.Arg.default(
    "snippet_lines", "Number of source lines shown per hit (0 to omit).",
    JSON_Schema.Format.int(minimum = Some(0)), 3)
  val located_in_arg = JSON_Schema.Arg.optional(
    "located_in", "Return entities located in these origins. Default: all.",
    JSON_Schema.Format.list(JSON_Schema.Format.string))
  val kinds_arg = JSON_Schema.Arg.default(
    "kinds", "Kinds to return. " +
    Find_Entities_Tool.Kind.values.filter(_.hint.nonEmpty)
      .map(kind => kind.name + " = " + kind.hint).mkString("; ") + ".",
    JSON_Schema.Format.list(
      JSON_Schema.Format.enumeration(Find_Entities_Tool.Kind.values, _.name)),
    List(Find_Entities_Tool.Kind.facts, Find_Entities_Tool.Kind.constant,
      Find_Entities_Tool.Kind.type_name, Find_Entities_Tool.Kind.`class`))

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(PIDE_AI_Tool_Schema.running_session_arg, PIDE_AI_Tool_Schema.origin_arg,
      PIDE_AI_Tool_Schema.start_line_arg, PIDE_AI_Tool_Schema.opt_end_line_arg,
      snippet_lines_arg, located_in_arg, kinds_arg))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: PIDE_AI_Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val cwd = context.cwd
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val node_name = PIDE_AI_Tool_Util.origin_param(session, args, cwd)
      val snapshot =
        PIDE_AI_Tool_Util.require_stable_node_snapshot(session, node_name, progress, cwd)
      val snippet_lines = snippet_lines_arg.get(args)
      val filter_node_names =
        located_in_arg.get(args).map(_.map(session.node_name(_, cwd)).toSet)
      val range = PIDE_AI_Util.check_text_range(Line.Document(snapshot.node.source),
        PIDE_AI_Tool_Schema.start_line_arg.get(args),
        PIDE_AI_Tool_Schema.opt_end_line_arg.get(args))
      val definition_kinds = kinds_arg.get(args).flatMap(_.markups)
      val definitions = PIDE_AI_Definition.definitions(
        session, snapshot, Some(range), snippet_lines, definition_kinds, filter_node_names,
        "The definition entry has not been loaded yet. " +
        PIDE_AI_Tool_Util.retry_soon_message)
      PIDE_AI_Definition.definitions_json(session.resources, definitions, cwd)
    }
}

class Find_Entities_Tools extends PIDE_AI_Tools(new Find_Entities_Tool)
