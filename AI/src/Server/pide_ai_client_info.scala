/*  Title:      AI/src/Server/pide_ai_client_info.scala
    Author:     Kevin Kappelmann

Client infos for PIDE AI server.
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Client_Info
extends PIDE_AI_Registry.Kind[PIDE_AI_Client_Info, PIDE_AI_Client_Infos] {
  val kind: String = "client info"
  val include_option: String = "pide_ai_client_infos_include"
  val exclude_option: String = "pide_ai_client_infos_exclude"
  def services: Class[PIDE_AI_Client_Infos] = classOf[PIDE_AI_Client_Infos]
  def id(info: PIDE_AI_Client_Info): String = info.id
}

class PIDE_AI_Client_Info(
  val id: String,
  val protocol: PIDE_AI_Protocol,
  val access_note: PIDE_AI_Connector[?] => Option[String])

class PIDE_AI_Client_Infos(infos: PIDE_AI_Client_Info*)
extends PIDE_AI_Registry.Entries(infos.toList)
