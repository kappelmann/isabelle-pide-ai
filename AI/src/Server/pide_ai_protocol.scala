/*  Title:      AI/src/Server/pide_ai_protocol.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Protocol {
  val kind: String = "protocol"

  def include_ids(options: Options): String = options.string("pide_ai_protocols_include")
  def exclude_ids(options: Options): String = options.string("pide_ai_protocols_exclude")

  def make_table(
    include_ids: String = PIDE_AI_Registry.all_option,
    exclude_ids: String = ""
  ): Map[String, PIDE_AI_Protocol] =
    PIDE_AI_Registry.make_table(kind,
      Isabelle_System.make_services(classOf[PIDE_AI_Protocols]).flatMap(_.entries), _.id,
      include_ids, exclude_ids)
}

class PIDE_AI_Protocol(
  val id: String,
  val endpoint: (PIDE_AI_Server, Line_Connection, Logger) => PIDE_AI_Endpoint)

class PIDE_AI_Protocols(protocols: PIDE_AI_Protocol*) extends Isabelle_System.Service {
  def entries: List[PIDE_AI_Protocol] = protocols.toList
}
