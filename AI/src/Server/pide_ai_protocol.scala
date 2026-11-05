/*  Title:      AI/src/Server/pide_ai_protocol.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Protocol extends PIDE_AI_Registry.Kind[PIDE_AI_Protocol, PIDE_AI_Protocols] {
  val kind: String = "protocol"
  val include_option: String = "pide_ai_protocols_include"
  val exclude_option: String = "pide_ai_protocols_exclude"
  def services: Class[PIDE_AI_Protocols] = classOf[PIDE_AI_Protocols]
  def id(protocol: PIDE_AI_Protocol): String = protocol.id
}

class PIDE_AI_Protocol(
  val id: String,
  val endpoint: (PIDE_AI_Server, Line_Connection, Logger) => PIDE_AI_Endpoint)

class PIDE_AI_Protocols(protocols: PIDE_AI_Protocol*)
extends PIDE_AI_Registry.Entries(protocols.toList)
