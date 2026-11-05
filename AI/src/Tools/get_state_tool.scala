/*  Title:      AI/src/Tools/get_state_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object Get_State_Tool {
  def get_state_delay(options: Options): Time =
    options.seconds("pide_ai_get_state_delay")

  enum Stop_On {
    case any_failed, all_terminated
    def stop(statuses: List[Document_Status.Command_Status]): Boolean =
      statuses.forall(_.is_terminated) || this == any_failed && statuses.exists(_.is_failed)
  }

  def base_description(which_commands: String): String =
    "Summary (errors, commands being processed,...) and optional details " +
    s"(subgoals, types, markup,...) of $which_commands. Limits do not affect counts. " +
    "Commands edited during request are omitted. " +
    "For a non-theory file, the state is that of the command loading it."

  object Request {
    val stop_on_arg = JSON_Schema.Arg.default("stop_on", "",
      JSON_Schema.Format.enumeration(Stop_On.values), Stop_On.any_failed)

    val include_summary_warnings_arg = JSON_Schema.Arg.default(
      "include_summary_warnings", "", JSON_Schema.Format.bool, false)
    val summary_problematic_limit_arg = JSON_Schema.Arg.optional(
      "summary_problematic_limit",
      "Problematic occurrences to return. Default: all.",
      JSON_Schema.Format.int(minimum = Some(0)))
    val summary_snippet_lines_arg = JSON_Schema.Arg.default(
      "summary_snippet_lines", "Number of source lines shown (0 for line ranges only).",
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

    def timeout_arg(default: Int) = JSON_Schema.Arg.default(
      "timeout", "Seconds to wait.",
      JSON_Schema.Format.int(minimum = Some(0)).map(Time.seconds(_), _.seconds.toInt),
      Time.seconds(default))

    def args(timeout_arg: JSON_Schema.Arg[Time]): List[JSON_Schema.Arg[?]] =
      List(include_summary_warnings_arg, summary_problematic_limit_arg, summary_snippet_lines_arg,
        details_arg, details_problematic_limit_arg, details_unproblematic_limit_arg, timeout_arg,
        stop_on_arg)

    def from_json(args: JSON.Object.T, timeout_arg: JSON_Schema.Arg[Time]): Request = {
      val summary_message_kinds = PIDE_AI_Message.Message_Kind.errors ::
        Option.when(include_summary_warnings_arg.get(args))(
          PIDE_AI_Message.Message_Kind.warnings).toList
      Request(timeout_arg.get(args), stop_on_arg.get(args),
        PIDE_AI_Document_Command.States.Report(details_arg.get(args).toSet, summary_message_kinds,
          details_problematic_limit_arg.get(args), details_unproblematic_limit_arg.get(args),
          summary_problematic_limit_arg.get(args), summary_snippet_lines_arg.get(args)))
    }
  }

  sealed case class Request(
    timeout: Time,
    stop_on: Stop_On,
    report: PIDE_AI_Document_Command.States.Report)

  def state(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    start_line: Int,
    opt_end_line: Option[Int],
    request: Request,
    progress: Progress
  ): JSON.Object.T = {
    val snapshot0 = PIDE_AI_Tool_Util.require_stable_origin_snapshot(session, node_name, progress)
    val range = Some(PIDE_AI_Util.check_text_range(
      Line.Document(snapshot0.node.source), start_line, opt_end_line))
    val (snapshot, states, edited, timed_out) =
      if (request.timeout.is_zero) (snapshot0,
        PIDE_AI_Document_Command.States(snapshot0, range, session.unicode_symbols), 0, false)
      else
        PIDE_AI_Document_Command.await_states(session, snapshot0, range, request.stop_on.stop,
          Some(request.timeout), get_state_delay(session.options), progress)
    PIDE_AI_Document_Command.States.json(states, Line.Document(snapshot.node.source), request.report) ++
    JSON_Object.flatten(Option.when(edited > 0)("cmds_edited_omitted" -> edited),
      Option.when(timed_out)("timed_out" -> true))
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
    progress: Progress
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val node_name = PIDE_AI_Tool_Util.origin_param(session, args)
      Get_State_Tool.state(session, node_name,
        PIDE_AI_Tool_Schema.start_line_default_arg.get(args),
        PIDE_AI_Tool_Schema.opt_end_line_arg.get(args),
        Get_State_Tool.Request.from_json(args, timeout_arg), progress)
    }
}

class Get_State_Tools extends PIDE_AI_Tools(new Get_State_Tool)
