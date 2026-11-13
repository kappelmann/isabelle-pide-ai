/*  Title:      AI/src/Tools/pide_ai_tool_schema.scala
    Author:     Kevin Kappelmann

Shared JSON-Schema fragments for tools.
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Tool_Schema {
  val implicit_load_files: String = "Loads files if needed."
  val range_perspective: String = "Moves visible PIDE perspective to range."
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
  def disk_sync_values_arg(
    values: Iterable[PIDE_AI_Tool_Util.Sync]
  ): JSON_Schema.Arg[PIDE_AI_Tool_Util.Sync] = {
    val default = PIDE_AI_Tool_Util.Sync.node
    require(values.exists(_ == default), s"Missing default disk_sync value $default")
    JSON_Schema.Arg.default("disk_sync", "Before call.", JSON_Schema.Format.enumeration(values),
      default)
  }
  val disk_sync_arg = disk_sync_values_arg(PIDE_AI_Tool_Util.Sync.values)
  val node_disk_sync_arg = disk_sync_values_arg(
    PIDE_AI_Tool_Util.Sync.values.filterNot(_ == PIDE_AI_Tool_Util.Sync.node_and_ancestors))

  def ms_format(minimum: Int): JSON_Schema.Format[Time] =
    JSON_Schema.Format.int(minimum = Some(minimum)).map(Time.ms(_), _.ms.toInt)
  val long_running_arg = JSON_Schema.Arg.optional("long_running_ms",
    s"Threshold. Default: ${PIDE_AI_Session.long_running_threshold_option}.", ms_format(0))
  val stop_on_arg = JSON_Schema.Arg.default("stop_on",
    s"${PIDE_AI_Tool_Util.Stop_On.any_failed} = first failure or all terminated.",
    JSON_Schema.Format.enumeration(PIDE_AI_Tool_Util.Stop_On.values),
    PIDE_AI_Tool_Util.Stop_On.any_failed)
  def progress_delay_arg(default: String): JSON_Schema.Arg[Option[Time]] =
    JSON_Schema.Arg.optional("progress_delay_ms", s"Between reports. Default: $default.",
      ms_format(0))

  def show_thy_statuses_arg(
    name: String,
    default: PIDE_AI_Theory_Status.Statuses.Show
  ): JSON_Schema.Arg[PIDE_AI_Theory_Status.Statuses.Show] =
    JSON_Schema.Arg.default(name, "Status, names, or nothing.",
      JSON_Schema.Format.enumeration(PIDE_AI_Theory_Status.Statuses.Show.values), default)
}
