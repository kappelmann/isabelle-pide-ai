/*  Title:      MCP/src/pide_ai_client_info.scala
    Author:     Kevin Kappelmann

Client infos for PIDE AI server.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_AI_Client_Info {
  val kind: String = "client info"

  def include_ids(options: Options): String = options.string("pide_mcp_client_infos_include")
  def exclude_ids(options: Options): String = options.string("pide_mcp_client_infos_exclude")

  def make_table(
    include_ids: String = PIDE_AI_Registry.all_option,
    exclude_ids: String = ""
  ): Map[String, PIDE_AI_Client_Info] =
    PIDE_AI_Registry.make_table(kind,
      Isabelle_System.make_services(classOf[PIDE_AI_Client_Infos]).flatMap(_.entries), _.id,
      include_ids, exclude_ids)
}

class PIDE_AI_Client_Info(
  val id: String,
  val protocol: PIDE_AI_Protocol,
  val access_note: PIDE_AI_Connector[?] => Option[String])

class PIDE_AI_Client_Infos(infos: PIDE_AI_Client_Info*) extends Isabelle_System.Service {
  def entries: List[PIDE_AI_Client_Info] = infos.toList
}
