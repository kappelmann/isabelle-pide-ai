/*  Title:      MCP/src/pide_mcp_endpoint.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Endpoint {
  val name = "isabelle_pide_mcp"
  val version = "0.1.0"
  val instructions = "Interactive, multi-session proof development with Isabelle."
}

class PIDE_MCP_Endpoint(server: PIDE_AI_Server, connection: Line_Connection, log: Logger)
extends PIDE_AI_Tool_Endpoint(server, connection, log) {
  private def respond(body: JSON.Object.T): Unit = connection.write_line(JSON.Format(body))

  private def respond_error(id: JSON_RPC.Id | Null, code: Int, message: String): Unit =
    respond(JSON_RPC.error(id, code, message))

  private enum Event {
    case Input(line: String) extends Event
    case Stop extends Event
    case Tool_Progress(id: JSON_RPC.Id,
      token: MCP.Progress_Token,
      serial: Long,
      message: String
    ) extends Event
    case Tool_Result(id: JSON_RPC.Id, result: PIDE_MCP_Tool_Result) extends Event
  }

  private val events = Mailbox[Event]()

  private var tool_calls = Map.empty[JSON_RPC.Id, (MCP.Tool_Call, PIDE_AI_Tool_Call)]
  private var cancelled = Set.empty[JSON_RPC.Id]
  private var stopping = false

  private def progress_token_owner(token: MCP.Progress_Token): Option[JSON_RPC.Id] =
    tool_calls.collectFirst { case (id, (call, _)) if call.progress_token.contains(token) => id }

  private def read_input(): Unit = {
    PIDE_MCP_Util.log_failure(log, "MCP input failure",
      Exn.capture { connection.read_lines().foreach(line => events.send(Event.Input(line))) })
    events.send(Event.Stop)
  }

  private def handle_cancelled(params: JSON.Object.T): Unit =
    Exn.result { MCP.read_cancellation(params) } match {
      case Exn.Res(cancellation) =>
        for ((_, task) <- tool_calls.get(cancellation.id) if !cancelled(cancellation.id)) {
          log("Cancelling tool call " + JSON.Format(cancellation.id) +
            cancellation.reason.map(reason =>
              s" (reason: ${JSON.Format(reason)})").getOrElse("") + "...")
          cancelled += cancellation.id
          task.stop()
        }
      case Exn.Exn(exn) =>
        log.error_message("Malformed cancellation notification: " + Exn.message(exn))
    }

  private def handle_initialize(id: JSON_RPC.Id, params: JSON.Object.T): Unit =
    Exn.result { MCP.read_initialize(params) } match {
      case Exn.Exn(exn) => respond_error(id, JSON_RPC.Error_Code.INVALID_PARAMS, Exn.message(exn))
      case Exn.Res(initialize) =>
        log(s"MCP client ${quote(initialize.client_name)} ${initialize.client_version} " +
          s"connected (protocol ${initialize.protocol_version})")
        respond(JSON_RPC.result(id,
          MCP.initialize_result(initialize, PIDE_MCP_Endpoint.name,
            PIDE_MCP_Endpoint.version,
            (PIDE_MCP_Endpoint.instructions :: server.access_note.toList).mkString(" "))))
    }

  private def handle_tools_list(id: JSON_RPC.Id): Unit =
    respond(JSON_RPC.result(id, JSON_Object("tools" -> server.tools.map(_.json))))

  private def handle_tool_call(id: JSON_RPC.Id, params: JSON.Object.T): Unit =
    Exn.result { MCP.read_tool_call(params) } match {
      case Exn.Exn(exn) => respond_error(id, JSON_RPC.Error_Code.INVALID_PARAMS, Exn.message(exn))
      case Exn.Res(call) => call.progress_token.flatMap(token =>
        progress_token_owner(token).map(token -> _)) match {
          case Some((token, owner)) => respond_error(id, JSON_RPC.Error_Code.INVALID_PARAMS,
            s"Progress token ${JSON.Format(token)} is already used " +
            s"for request ${JSON.Format(owner)}")
          case None => server.tool(call.name) match {
            case None => respond_error(id, JSON_RPC.Error_Code.INVALID_PARAMS,
              tool_not_found(call.name))
            case Some(tool) =>
              val task = start_tool_call(tool, call.args,
                call.progress_token.map(token => (serial, message) =>
                  events.send(Event.Tool_Progress(id, token, serial, message))),
                result => events.send(Event.Tool_Result(id, result)))
              tool_calls += id -> (call, task)
          }
        }
    }

  private def handle_message(json: JSON.T): Unit =
    Exn.result {
      Exn.result { JSON_RPC.read_message(json) } match {
        case Exn.Exn(exn) => respond_error(JSON_RPC.id(json).getOrElse(null), 
          JSON_RPC.Error_Code.INVALID_REQUEST, "Bad request: " + Exn.message(exn))
        case Exn.Res(JSON_RPC.Message.Notification(method, params)) =>
          if (method == MCP.Method.notifications_cancelled) handle_cancelled(params)
        case Exn.Res(JSON_RPC.Message.Request(_, _, _)) if stopping =>
        case Exn.Res(JSON_RPC.Message.Request(id, _, _)) if tool_calls.contains(id) =>
          respond_error(id, JSON_RPC.Error_Code.INVALID_REQUEST,
            s"Request id ${JSON.Format(id)} is already active")
        case Exn.Res(JSON_RPC.Message.Request(id, method, params)) =>
          method match {
            case MCP.Method.initialize => handle_initialize(id, params)
            case MCP.Method.ping => respond(JSON_RPC.result(id, JSON.Object.empty))
            case MCP.Method.tools_list => handle_tools_list(id)
            case MCP.Method.tools_call => handle_tool_call(id, params)
            case _ => respond_error(id, JSON_RPC.Error_Code.METHOD_NOT_FOUND,
              s"Method not found: ${quote(method)}")
          }
      }
    } match {
      case Exn.Res(_) =>
      case Exn.Exn(exn) =>
        val id = JSON_RPC.id(json)
        val message = PIDE_MCP_Util.handle_internal_error(log,
          id.fold("notification")(id => "request " + JSON.Format(id)), exn)
        for (id <- id)
          Exn.result { respond_error(id, JSON_RPC.Error_Code.INTERNAL_ERROR, message) }
    }

  private def handle_input(line: String): Unit =
    Exn.result { JSON.parse(line, strict = false) } match {
      case Exn.Exn(exn) =>
        log(s"Parse error: ${Exn.message(exn)}")
        respond_error(null, JSON_RPC.Error_Code.PARSE_ERROR, s"Parse error: ${Exn.message(exn)}")
      case Exn.Res(json) => handle_message(json)
    }

  protected def serve(): Unit = {
    Isabelle_Thread.fork(name = "pide_mcp_input", daemon = true) { read_input() }

    while (!stopping || tool_calls.nonEmpty) {
      for (event <- events.receive()) event match {
        case Event.Input(line) => handle_input(line)
        case Event.Stop =>
          stopping = true
          tool_call_scope.stop()
        case Event.Tool_Progress(id, token, serial, message) =>
          require(tool_calls.isDefinedAt(id), s"Active tool call expected: ${JSON.Format(id)}")
          if (!cancelled(id)) respond(MCP.progress_notification(token, serial, message))
        case Event.Tool_Result(id, tool_result) =>
          require(tool_calls.isDefinedAt(id), s"Active tool call expected: ${JSON.Format(id)}")
          tool_calls -= id
          if (cancelled(id)) cancelled -= id
          else respond(JSON_RPC.result(id, MCP.tool_result(tool_result)))
      }
    }
  }

  def stop(): Unit = events.send(Event.Stop)
}
