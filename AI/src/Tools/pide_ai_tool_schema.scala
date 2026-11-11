/*  Title:      AI/src/Tools/pide_ai_tool_schema.scala
    Author:     Kevin Kappelmann

Shared JSON-Schema fragments for tools.
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Tool_Schema {
  val implicit_load_files: String = "Loads files if needed."
  val implicit_sync: String = "Syncs from disk."
  val range_perspective: String = "Moves PIDE perspective to range."
  val no_processing_wait: String = "Does not wait for commands to terminate."

  val running_session_arg = JSON_Schema.Arg.optional(
    "session_id", "Default: unique running one.",
    JSON_Schema.Format.string)
  val running_sessions_arg = JSON_Schema.Arg.optional(
    "session_ids", "Default: all running ones.",
    JSON_Schema.Format.list(JSON_Schema.Format.string))
  val node_arg = JSON_Schema.Arg.required("node",
    s"Session-qualified theory (e.g. ${quote("HOL.Nat")}) or file path.",
    JSON_Schema.Format.string)
  val start_line_arg = JSON_Schema.Arg.required(
    "start_line", "", JSON_Schema.Format.int(minimum = Some(1)))
  val start_line_default_arg = JSON_Schema.Arg.default(
    "start_line", "", JSON_Schema.Format.int(minimum = Some(1)), 1)
  val opt_end_line_arg = JSON_Schema.Arg.optional(
    "end_line", "Default: end of file.", JSON_Schema.Format.int(minimum = Some(1)))
  val disk_sync_arg = JSON_Schema.Arg.default("disk_sync", "Before call.",
    JSON_Schema.Format.enumeration(PIDE_AI_Tool_Util.Sync.values), PIDE_AI_Tool_Util.Sync.node)

  def ms_format(minimum: Int): JSON_Schema.Format[Time] =
    JSON_Schema.Format.int(minimum = Some(minimum)).map(Time.ms(_), _.ms.toInt)
  val long_running_arg = JSON_Schema.Arg.optional("long_running_ms",
    s"Threshold. Default: ${PIDE_AI_Session.long_running_threshold_option}.", ms_format(0))

  def show_thy_statuses_arg(
    name: String,
    default: PIDE_AI_Theory_Status.Statuses.Show
  ): JSON_Schema.Arg[PIDE_AI_Theory_Status.Statuses.Show] =
    JSON_Schema.Arg.default(name, "Status, names, or nothing.",
      JSON_Schema.Format.enumeration(PIDE_AI_Theory_Status.Statuses.Show.values), default)
}
