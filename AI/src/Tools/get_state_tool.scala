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
  val get_state_progress_delay_option: String = "pide_ai_get_state_progress_delay"
  def get_state_progress_delay(options: Options): Time =
    options.seconds(get_state_progress_delay_option)

  enum Stop_On {
    case any_failed, all_terminated
    def stop(statuses: List[Document_Status.Command_Status]): Boolean =
      statuses.forall(_.is_terminated) || this == any_failed && statuses.exists(_.is_failed)
  }

  enum Show_Thys {
    case none, failed, not_ok, all
    def select(statuses: => List[PIDE_AI_Theory_Status]): List[PIDE_AI_Theory_Status] =
      this match {
        case `none` => Nil
        case `failed` => statuses.filter(_.is_failed)
        case `not_ok` => statuses.filterNot(_.is_ok)
        case `all` => statuses
      }
  }

  val other_cmds_running_key: String =
    "other_" + PIDE_AI_Document_Command.States.cmds_key(PIDE_AI_Command_Status.running.name)

  enum Show_Other_Cmds_Running {
    case never, if_range_unterminated, always
    def show(unterminated: Boolean): Boolean =
      this == always || this == if_range_unterminated && unterminated
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
      "Problematic items to return. Default: all.",
      JSON_Schema.Format.int(minimum = Some(0)))
    val snippet_lines_arg = JSON_Schema.Arg.default(
      "snippet_lines", "Number of source lines shown (0 for line ranges only).",
      JSON_Schema.Format.int(minimum = Some(0)), 1)
    val details_arg = JSON_Schema.Arg.default(
      "details",
      "Details to return. infos = " + commas(PIDE_AI_Document_Command.State.Detail.kinds(
        PIDE_AI_Document_Command.State.Detail.infos).map(PIDE_AI_Message.Messages.key)) + ".",
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
    val show_other_cmds_running_arg = JSON_Schema.Arg.default(
      "show_" + other_cmds_running_key, "",
      JSON_Schema.Format.enumeration(Show_Other_Cmds_Running.values),
      Show_Other_Cmds_Running.if_range_unterminated)

    def timeout_arg(default: Int) = JSON_Schema.Arg.default(
      "timeout", "Seconds to wait.",
      JSON_Schema.Format.int(minimum = Some(0)).map(Time.seconds(_), _.seconds.toInt),
      Time.seconds(default))
    val progress_delay_arg = JSON_Schema.Arg.optional("progress_delay",
      s"Seconds between reports. Default: $get_state_progress_delay_option.",
      PIDE_AI_Tool_Schema.seconds_format)

    def args(timeout_arg: JSON_Schema.Arg[Time]): List[JSON_Schema.Arg[?]] =
      List(show_summary_warnings_arg, summary_problematic_limit_arg, snippet_lines_arg,
        details_arg, details_problematic_limit_arg, details_unproblematic_limit_arg,
        show_ancestor_thys_arg, show_descendant_thys_arg, show_other_cmds_running_arg,
        PIDE_AI_Tool_Schema.long_running_arg, timeout_arg, stop_on_arg,
        progress_delay_arg)

    def from_json(args: JSON.Object.T, timeout_arg: JSON_Schema.Arg[Time]): Request = {
      val message_kinds = PIDE_AI_Message.Kind.error ::
        Option.when(show_summary_warnings_arg.get(args))(PIDE_AI_Message.Kind.warning).toList
      Request(timeout_arg.get(args), stop_on_arg.get(args),
        PIDE_AI_Document_Command.States.Show(details_arg.get(args).toSet, message_kinds,
          details_problematic_limit_arg.get(args), details_unproblematic_limit_arg.get(args),
          summary_problematic_limit_arg.get(args), snippet_lines_arg.get(args)),
        show_ancestor_thys_arg.get(args), show_descendant_thys_arg.get(args),
        show_other_cmds_running_arg.get(args),
        PIDE_AI_Tool_Schema.long_running_arg.get(args), progress_delay_arg.get(args))
    }
  }

  sealed case class Request(
    timeout: Time,
    stop_on: Stop_On,
    show: PIDE_AI_Document_Command.States.Show,
    show_ancestor_thys: Show_Thys,
    show_descendant_thys: Show_Thys,
    show_other_cmds_running: Show_Other_Cmds_Running,
    long_running_threshold: Option[Time],
    progress_delay: Option[Time])

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
    val resources = session.resources
    val long_running_threshold = request.long_running_threshold.getOrElse(
      PIDE_AI_Session.long_running_threshold(session.options))

    def json(
      snapshot: Document.Snapshot,
      states: List[PIDE_AI_Document_Command.State],
      edited: Int,
      timed_out: Boolean
    ): JSON.Object.T = {
      val nodes = snapshot.version.nodes
      val states_json = PIDE_AI_Document_Command.States.json(states, request.show,
        Line.Document(snapshot.node.source), long_running_threshold, session.unicode_symbols, cwd)
      val unterminated = states.exists(!_.command_status.is_terminated)
      lazy val dynamic_thys = PIDE_AI_Nodes.Loaded(resources, nodes).dynamic
      val thys = PIDE_AI_Nodes.node_theory(nodes, node_name).toList
      val blocked = PIDE_AI_Nodes.blocked_by(resources, snapshot)
      def statuses(names: Set[Document.Node.Name]): List[PIDE_AI_Theory_Status] =
        PIDE_AI_Theory_Status.Statuses(snapshot, dynamic_thys.filter(names -- thys))
      val ancestors = request.show_ancestor_thys.select(
        statuses(PIDE_AI_Dependencies.known_dependencies(resources, nodes, thys).toSet))
      val descendants =
        request.show_descendant_thys.select(statuses(nodes.descendants(thys).toSet))
      val other_cmds_running =
        if (!request.show_other_cmds_running.show(unterminated)) JSON.Object.empty
        else {
          val cmds = selection.parts.map(_._1).toSet
          val selections =
            PIDE_AI_Document_Command.Selection.nodes(session, snapshot, dynamic_thys)
              .map((node_snapshot, node_selection) =>
                node_snapshot -> node_selection.filter((cmd, _) => !cmds(cmd)))
          PIDE_AI_Document_Command.nodes_running_json(resources, selections,
            1, long_running_threshold, cwd)
        }
      states_json ++ JSON_Object.flatten(Option.when(edited > 0)(
        PIDE_AI_Document_Command.States.cmds_key("edited_omitted") -> edited),
        Option.when(timed_out)("timed_out" -> true),
        Option.when(blocked.nonEmpty)("blocked_by" -> JSON_Object(blocked.map((name, header) =>
          PIDE_AI_Origin.print(resources, name, cwd) ->
            header.errors.map(_.text(session.unicode_symbols, cwd))))),
        Option.when(ancestors.nonEmpty)(
          "ancestors" -> PIDE_AI_Theory_Status.Statuses.json(resources, ancestors,
            session.unicode_symbols, cwd)),
        Option.when(descendants.nonEmpty)(
          "descendants" -> PIDE_AI_Theory_Status.Statuses.json(resources, descendants,
            session.unicode_symbols, cwd)),
        Option.when(other_cmds_running.nonEmpty)(
          other_cmds_running_key -> other_cmds_running))
    }

    if (request.timeout.is_zero)
      json(snapshot0, PIDE_AI_Document_Command.States(snapshot0, selection).toList, 0, false)
    else {
      val (snapshot, states, edited, timed_out) =
        PIDE_AI_Document_Command.await_states(session, snapshot0, selection,
          (snapshot, statuses) => request.stop_on.stop(statuses) ||
            PIDE_AI_Nodes.blocked_by(resources, snapshot.switch(node_name)).nonEmpty,
          Some((request.timeout - (Time.now() - start)) max Time.zero),
          get_state_delay(session.options), get_state_check_factor(session.options),
          (snapshot, states) => JSON.Format(json(snapshot, states.toList, 0, false)),
          request.progress_delay.getOrElse(get_state_progress_delay(session.options)), progress)
      json(snapshot, states.toList, edited, timed_out)
    }
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
