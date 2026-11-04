/*  Title:      MCP/src/json_rpc.scala
    Author:     Kevin Kappelmann

JSON-RPC 2.0 messages.
*/

package isabelle.pide.mcp

import isabelle._
import isabelle.pide.ai._

object JSON_RPC {
  val version = "2.0"

  type Id = String | Long
  object Id {
    def unapply(value: JSON.T): Option[Id] =
      value match {
        case string: String => Some(string)
        case JSON.Value.Long(long) => Some(long)
        case _ => None
      }
    val format: JSON_Schema.Format[Id] = JSON_Schema.Format(
      JSON_Object("type" -> List("string", "integer")), unapply, identity[Id])
  }

  object Error_Code {
    // JSON-RPC 2.0 reserved error codes (https://www.jsonrpc.org/specification#error_object)
    val PARSE_ERROR = -32700
    val INVALID_REQUEST = -32600
    val METHOD_NOT_FOUND = -32601
    val INVALID_PARAMS = -32602
    val INTERNAL_ERROR = -32603
  }

  val jsonrpc_arg =
    JSON_Schema.Arg.required("jsonrpc", "", JSON_Schema.Format.enumeration(List(version)))
  val id_arg = JSON_Schema.Arg.optional("id", "", Id.format)
  val method_arg = JSON_Schema.Arg.required("method", "", JSON_Schema.Format.string)
  val params_arg =
    JSON_Schema.Arg.default("params", "", JSON_Schema.Format.obj, JSON.Object.empty)
  val response_id_arg =
    JSON_Schema.Arg.required("id", "", JSON_Schema.Format.nullable(Id.format))
  val result_arg = JSON_Schema.Arg.required("result", "", JSON_Schema.Format.obj)
  val error_arg = JSON_Schema.Arg.required("error", "", JSON_Schema.Format.obj)
  val code_arg = JSON_Schema.Arg.required("code", "", JSON_Schema.Format.int())
  val message_arg = JSON_Schema.Arg.required("message", "", JSON_Schema.Format.string)

  enum Message {
    case Request(id: Id, method: String, params: JSON.Object.T)
    case Notification(method: String, params: JSON.Object.T)
  }

  def read_message(json: JSON.T): Message =
    json match {
      case JSON.Object(obj) =>
        jsonrpc_arg.get(obj)
        val method = method_arg.get(obj)
        val params = params_arg.get(obj)
        id_arg.get(obj) match {
          case Some(id) => Message.Request(id, method, params)
          case None => Message.Notification(method, params)
        }
      case _ => isabelle.error("expected a JSON object")
    }

  def id(json: JSON.T): Option[Id] = JSON.value(json, id_arg.name, Id.unapply)

  def result(id: Id, result: JSON.Object.T): JSON.Object.T =
    jsonrpc_arg(version) ++ response_id_arg(id) ++ result_arg(result)

  def error(id: Id | Null, code: Int, message: String): JSON.Object.T =
    jsonrpc_arg(version) ++ response_id_arg(id) ++
    error_arg(code_arg(code) ++ message_arg(message))

  def notification(method: String, params: JSON.Object.T): JSON.Object.T =
    jsonrpc_arg(version) ++ method_arg(method) ++ params_arg(params)
}
