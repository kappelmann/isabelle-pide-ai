/*  Title:      MCP/src/pide_mcp_service.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Service {
  val kind: String = "service"

  val max_id_length: Int = 128
  def valid_id(id: String): Boolean =
    id.nonEmpty && id.length <= max_id_length &&
    id.forall(c => Symbol.is_ascii_letter(c) || Symbol.is_ascii_digit(c) || "_-.".contains(c))

  def include_ids(options: Options): String = options.string("pide_mcp_services_include")
  def exclude_ids(options: Options): String = options.string("pide_mcp_services_exclude")

  def make_table(
    include_ids: String = PIDE_AI_Registry.all_option,
    exclude_ids: String = ""
  ): Map[String, PIDE_MCP_Service] =
    PIDE_AI_Registry.make_table(kind,
      Isabelle_System.make_services(classOf[PIDE_MCP_Services]).flatMap(_.entries), _.id,
      include_ids, exclude_ids)

  sealed case class Session_Event(
    server: PIDE_AI_Server,
    session: PIDE_MCP_Session,
    progress: Progress)
}

abstract class PIDE_MCP_Service(val id: String)
extends Lifecycle_Hooks[PIDE_MCP_Service.Session_Event, PIDE_MCP_Service.Session_Event] {
  def kind: String = PIDE_MCP_Service.kind

  if (!PIDE_MCP_Service.valid_id(id)) error(s"Bad PIDE_MCP $kind id: ${quote(id)}")

  def description: String

  def json: JSON.Object.T = JSON_Object("name" -> id, "description" -> description)
}

class PIDE_MCP_Services(services: PIDE_MCP_Service*) extends Isabelle_System.Service {
  def entries: List[PIDE_MCP_Service] = services.toList
}
