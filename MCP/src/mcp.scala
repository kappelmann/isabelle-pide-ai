/*  Title:      MCP/src/mcp.scala
    Author:     Kevin Kappelmann

Model Context Protocol.
*/

package isabelle.pide.mcp

import isabelle._

object MCP {
  val protocol_version = "2025-11-25"

  object Method {
    val initialize = "initialize"
    val ping = "ping"
    val tools_list = "tools/list"
    val tools_call = "tools/call"
    val notifications_progress = "notifications/progress"
    val notifications_cancelled = "notifications/cancelled"
  }

  val meta_arg = JSON_Schema.Arg.default("_meta", "", JSON_Schema.Format.obj, JSON.Object.empty)
  val progress_token_arg = JSON_Schema.Arg.optional("progressToken", "", JSON_RPC.Id.format)
  val capabilities_arg = JSON_Schema.Arg.required("capabilities", "", JSON_Schema.Format.obj)
  val client_info_arg = JSON_Schema.Arg.required("clientInfo", "", JSON_Schema.Format.obj)
  val name_arg = JSON_Schema.Arg.required("name", "", JSON_Schema.Format.string)
  val version_arg = JSON_Schema.Arg.required("version", "", JSON_Schema.Format.string)
  val protocol_version_arg =
    JSON_Schema.Arg.required("protocolVersion", "", JSON_Schema.Format.string)
  val request_id_arg = JSON_Schema.Arg.required("requestId", "", JSON_RPC.Id.format)
  val reason_arg = JSON_Schema.Arg.optional("reason", "", JSON_Schema.Format.string)
  val progress_arg = JSON_Schema.Arg.required("progress", "", JSON_Schema.Format.long)
  val message_arg = JSON_Schema.Arg.required("message", "", JSON_Schema.Format.string)
  val arguments_arg =
    JSON_Schema.Arg.default("arguments", "", JSON_Schema.Format.obj, JSON.Object.empty)

  sealed case class Initialize(
    protocol_version: String,
    client_name: String,
    client_version: String
  )

  def read_initialize(params: JSON.Object.T): Initialize = {
    capabilities_arg.get(params)
    val (client_name, client_version) =
      client_info_arg.within(params)(info => (name_arg.get(info), version_arg.get(info)))
    Initialize(protocol_version_arg.get(params), client_name, client_version)
  }

  // MCP negotiation: answer with client version if supported;
  // otherwise return the server's supported version
  // TODO: do we support other versions?
  def decide_protocol_version(client_protocol_version: String): String =
    protocol_version

  def initialize_result(
    initialize: Initialize,
    server_name: String,
    server_version: String,
    instructions: String
  ): JSON.Object.T =
    JSON_Object(
      "protocolVersion" -> decide_protocol_version(initialize.protocol_version),
      "capabilities" -> JSON_Object("tools" -> JSON_Object()),
      "serverInfo" -> JSON_Object("name" -> server_name, "version" -> server_version),
      "instructions" -> instructions)

  type Progress_Token = JSON_RPC.Id

  sealed case class Tool_Call(
    name: String,
    args: JSON.Object.T,
    progress_token: Option[Progress_Token]
  )

  private def read_progress_token(params: JSON.Object.T): Option[Progress_Token] =
    meta_arg.within(params)(progress_token_arg.get)

  def read_tool_call(params: JSON.Object.T): Tool_Call =
    Tool_Call(name_arg.get(params), arguments_arg.get(params), read_progress_token(params))

  def progress_notification(
    token: Progress_Token,
    serial: Long,
    message: String
  ): JSON.Object.T =
    JSON_RPC.notification(Method.notifications_progress,
      progress_token_arg(Some(token)) ++ progress_arg(serial) ++ message_arg(message))

  def text_content(text: String): JSON.Object.T =
    JSON_Object("type" -> "text", "text" -> text)

  def tool_result(result: Result[JSON.T, JSON.T]): JSON.Object.T = {
    val (json, is_error) = result match {
      case Result.Res(res) => (res, false)
      case Result.Error(err) => (err, true)
    }
    val structured = json match {
      case JSON.Object(obj) => obj
      case _ => JSON_Object("result" -> json)
    }
    // MCP spec: needed for errors + should include for non-errors for backwards compatibility
    val content = List(text_content(JSON_Object.text(json)))
    JSON_Object("content" -> content, "structuredContent" -> structured) ++
    JSON.optional("isError" -> Option.when(is_error)(true))
  }

  sealed case class Cancellation(id: JSON_RPC.Id, reason: Option[String])

  def read_cancellation(params: JSON.Object.T): Cancellation =
    Cancellation(request_id_arg.get(params), reason_arg.get(params))
}
