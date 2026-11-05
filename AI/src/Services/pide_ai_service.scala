/*  Title:      AI/src/Services/pide_ai_service.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Service extends PIDE_AI_Registry.Kind[PIDE_AI_Service, PIDE_AI_Services] {
  val kind: String = "service"
  val include_option: String = "pide_ai_services_include"
  val exclude_option: String = "pide_ai_services_exclude"
  def services: Class[PIDE_AI_Services] = classOf[PIDE_AI_Services]
  def id(service: PIDE_AI_Service): String = service.id

  val max_id_length: Int = 128
  def valid_id(id: String): Boolean =
    id.nonEmpty && id.length <= max_id_length &&
    id.forall(c => Symbol.is_ascii_letter(c) || Symbol.is_ascii_digit(c) || "_-.".contains(c))

  sealed case class Session_Event(
    server: PIDE_AI_Server,
    session: PIDE_AI_Session,
    progress: Progress)
}

abstract class PIDE_AI_Service(val id: String)
extends Lifecycle_Hooks[PIDE_AI_Service.Session_Event, PIDE_AI_Service.Session_Event] {
  def kind: String = PIDE_AI_Service.kind

  if (!PIDE_AI_Service.valid_id(id)) error(s"Bad PIDE_AI $kind id: ${quote(id)}")

  def description: String

  def json: JSON.Object.T = JSON_Object("name" -> id, "description" -> description)
}

class PIDE_AI_Services(services: PIDE_AI_Service*)
extends PIDE_AI_Registry.Entries(services.toList)
