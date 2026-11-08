/*  Title:      AI/src/Tools/get_state_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object Get_State_Tool {
  def get_state_delay(options: Options): Time =
    options.seconds("pide_ai_get_state_delay")
  def get_state_check_factor(options: Options): Double =
    options.real("pide_ai_get_state_check_factor")

  enum Stop_On {
    case any_failed, all_terminated
    def stop(statuses: List[Document_Status.Command_Status]): Boolean =
      statuses.forall(_.is_terminated) || this == any_failed && statuses.exists(_.is_failed)
  }

  enum Show_Thys {
    case none, failed, not_ok, all
    def select(statuses: => List[PIDE_AI_Theory.Status]): List[PIDE_AI_Theory.Status] =
      this match {
        case `none` => Nil
        case `failed` => statuses.filter(_.is_failed)
        case `not_ok` => statuses.filterNot(_.is_ok)
        case `all` => statuses
      }
  }

  val other_cmds_processing_key: String =
    "other_" + PIDE_AI_Document_Command.Status.key(PIDE_AI_Document_Command.Status.running.name)

  enum Show_Other_Cmds_Processing {
    case never, if_range_unfinished, always
    def show(range_summary: JSON.Object.T): Boolean =
      this == always || this == if_range_unfinished &&
        List(PIDE_AI_Document_Command.Status.unprocessed, PIDE_AI_Document_Command.Status.running)
          .exists(status =>
            range_summary.contains(PIDE_AI_Document_Command.Status.key(status.name)))
  }

  def base_description(which_commands: String): String =
    "Summary (errors, commands being processed,...) and optional details " +
    s"(subgoals, types, markup,...) of $which_commands. Limits do not affect counts. " +
    "Commands edited during request are omitted. " +
    "For a non-theory file, the state is that of the command loading it."

  object Request {
    val stop_on_arg = JSON_Schema.Arg.default("stop_on", "",
      JSON_Schema.Format.enumeration(Stop_On.values), Stop_On.any_failed)

    val show_summary_warnings_arg = JSON_Schema.Arg.default(
      "show_summary_warnings", "", JSON_Schema.Format.bool, false)
    val summary_problematic_limit_arg = JSON_Schema.Arg.optional(
      "summary_problematic_limit",
      "Problematic occurrences to return. Default: all.",
      JSON_Schema.Format.int(minimum = Some(0)))
    val snippet_lines_arg = JSON_Schema.Arg.default(
      "snippet_lines", "Number of source lines shown (0 for line ranges only).",
      JSON_Schema.Format.int(minimum = Some(0)), 1)
    val details_arg = JSON_Schema.Arg.default(
      "details",
      s"Details to return. infos = ${commas(PIDE_AI_Message.Message_Kind.info.map(_.toString))}.",
      JSON_Schema.Format.list(
        JSON_Schema.Format.enumeration(PIDE_AI_Document_Command.State.Detail.values)),
      Nil)
    val details_problematic_limit_arg = JSON_Schema.Arg.optional(
      "details_problematic_limit",
      "Problematic commands to return. Default: all.",
      JSON_Schema.Format.int(minimum = Some(0)))
    val details_unproblematic_limit_arg = JSON_Schema.Arg.optional(
      "details_unproblematic_limit",
      "Unproblematic commands to return. Default: all.",
      JSON_Schema.Format.int(minimum = Some(0)))
    val show_ancestor_thys_arg = JSON_Schema.Arg.default(
      "show_ancestor_thys", "Summary of dynamic ancestors.",
      JSON_Schema.Format.enumeration(Show_Thys.values), Show_Thys.not_ok)
    val show_descendant_thys_arg = JSON_Schema.Arg.default(
      "show_descendant_thys", "Summary of descendants.",
      JSON_Schema.Format.enumeration(Show_Thys.values), Show_Thys.none)
    val show_other_cmds_processing_arg = JSON_Schema.Arg.default(
      "show_" + other_cmds_processing_key, "",
      JSON_Schema.Format.enumeration(Show_Other_Cmds_Processing.values),
      Show_Other_Cmds_Processing.if_range_unfinished)

    def timeout_arg(default: Int) = JSON_Schema.Arg.default(
      "timeout", "Seconds to wait.",
      JSON_Schema.Format.int(minimum = Some(0)).map(Time.seconds(_), _.seconds.toInt),
      Time.seconds(default))

    def args(timeout_arg: JSON_Schema.Arg[Time]): List[JSON_Schema.Arg[?]] =
      List(show_summary_warnings_arg, summary_problematic_limit_arg, snippet_lines_arg,
        details_arg, details_problematic_limit_arg, details_unproblematic_limit_arg,
        show_ancestor_thys_arg, show_descendant_thys_arg, show_other_cmds_processing_arg,
        timeout_arg, stop_on_arg)

    def from_json(args: JSON.Object.T, timeout_arg: JSON_Schema.Arg[Time]): Request = {
      val summary_message_kinds = PIDE_AI_Message.Message_Kind.errors ::
        Option.when(show_summary_warnings_arg.get(args))(
          PIDE_AI_Message.Message_Kind.warnings).toList
      Request(timeout_arg.get(args), stop_on_arg.get(args),
        PIDE_AI_Document_Command.States.Report(details_arg.get(args).toSet, summary_message_kinds,
          details_problematic_limit_arg.get(args), details_unproblematic_limit_arg.get(args),
          summary_problematic_limit_arg.get(args), snippet_lines_arg.get(args)),
        show_ancestor_thys_arg.get(args), show_descendant_thys_arg.get(args),
        show_other_cmds_processing_arg.get(args))
    }
  }

  sealed case class Request(
    timeout: Time,
    stop_on: Stop_On,
    report: PIDE_AI_Document_Command.States.Report,
    show_ancestor_thys: Show_Thys,
    show_descendant_thys: Show_Thys,
    show_other_cmds_processing: Show_Other_Cmds_Processing)

  def state(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    start_line: Int,
    opt_end_line: Option[Int],
    request: Request,
    progress: Progress,
    cwd: Option[Path]
  ): JSON.Object.T = {
    val start = Time.now()
    val snapshot0 =
      PIDE_AI_Tool_Util.require_stable_node_snapshot(session, node_name, progress, cwd)
    val range = PIDE_AI_Util.check_text_range(
      Line.Document(snapshot0.node.source), start_line, opt_end_line)
    val selection = PIDE_AI_Document_Command.Selection(snapshot0, Some(range))
    val (snapshot, states, edited, timed_out) =
      if (request.timeout.is_zero) (snapshot0,
        PIDE_AI_Document_Command.States(snapshot0, selection, session.unicode_symbols), 0, false)
      else
        PIDE_AI_Document_Command.await_states(session, snapshot0, selection,
          request.stop_on.stop, Some((request.timeout - (Time.now() - start)) max Time.zero),
          get_state_delay(session.options), get_state_check_factor(session.options), progress)
    val resources = session.resources
    val nodes = snapshot.version.nodes
    val summary = PIDE_AI_Document_Command.States.json(snapshot, states, request.report, cwd)
    lazy val dynamic_thys = PIDE_AI_Nodes.Loaded(resources, nodes).dynamic
    val thys = PIDE_AI_Nodes.node_theory(nodes, node_name).toList
    def statuses(names: Set[Document.Node.Name]): List[PIDE_AI_Theory.Status] =
      PIDE_AI_Theory.Status.statuses(snapshot, dynamic_thys.filter(names -- thys))
    val ancestors = request.show_ancestor_thys.select(
      statuses(PIDE_AI_Dependencies.known_dependencies(resources, nodes, thys).toSet))
    val descendants =
      request.show_descendant_thys.select(statuses(nodes.descendants(thys).toSet))
    val other_cmds_processing =
      if (!request.show_other_cmds_processing.show(summary)) JSON.Object.empty
      else {
        val cmds = selection.parts.map(_._1).toSet
        val selections = PIDE_AI_Document_Command.Selection.nodes(session, snapshot, dynamic_thys)
          .map((node_snapshot, node_selection) =>
            node_snapshot -> node_selection.filter((cmd, _) => !cmds(cmd)))
        val node_problems = PIDE_AI_Document_Command.status_problems(selections,
          PIDE_AI_Document_Command.Status.running, session.unicode_symbols)
        PIDE_AI_Document_Command.node_problems_json(resources, node_problems, 1, cwd)
      }
    summary ++ JSON_Object.flatten(Option.when(edited > 0)("cmds_edited_omitted" -> edited),
      Option.when(timed_out)("timed_out" -> true),
      Option.when(ancestors.nonEmpty)(
        "ancestors" -> PIDE_AI_Theory.Status.statuses_json(resources, ancestors, cwd)),
      Option.when(descendants.nonEmpty)(
        "descendants" -> PIDE_AI_Theory.Status.statuses_json(resources, descendants, cwd)),
      Option.when(other_cmds_processing.nonEmpty)(
        other_cmds_processing_key -> other_cmds_processing))
  }
}

class Get_State_Tool extends PIDE_AI_Tool("get_state") with PIDE_AI_Tool.Read_Only {
  def description: String =
    Get_State_Tool.base_description("commands in range") + " " +
    PIDE_AI_Tool_Schema.implicit_load_file

  val timeout_arg = Get_State_Tool.Request.timeout_arg(0)

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(PIDE_AI_Tool_Schema.running_session_arg, PIDE_AI_Tool_Schema.origin_arg,
      PIDE_AI_Tool_Schema.start_line_default_arg, PIDE_AI_Tool_Schema.opt_end_line_arg) :::
    Get_State_Tool.Request.args(timeout_arg))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val node_name = PIDE_AI_Tool_Util.origin_param(session, args, context.cwd)
      Get_State_Tool.state(session, node_name,
        PIDE_AI_Tool_Schema.start_line_default_arg.get(args),
        PIDE_AI_Tool_Schema.opt_end_line_arg.get(args),
        Get_State_Tool.Request.from_json(args, timeout_arg), progress, context.cwd)
    }
}

class Get_State_Tools extends PIDE_AI_Tools(new Get_State_Tool)
