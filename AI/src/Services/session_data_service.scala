/*  Title:      AI/src/Services/session_data_service.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

trait Session_Data_Service[D] extends PIDE_AI_Service {
  def start_data(event: PIDE_AI_Service.Session_Event): D
  def stop_data(event: PIDE_AI_Service.Session_Event, data: D): Unit

  private val session_data = Synchronized(Map.empty[String, D])

  def data(session: PIDE_AI_Session): Option[D] = session_data.value.get(session.id)

  override def started(event: PIDE_AI_Service.Session_Event): Unit = {
    super.started(event)
    val started_data = start_data(event)
    session_data.change(_ + (event.session.id -> started_data))
  }

  override def stopping(event: PIDE_AI_Service.Session_Event): Unit = {
    val id = event.session.id
    val stopped_data = session_data.change_result(all => (all.get(id), all - id))
    PIDE_AI_Util.release_combined(List(
      Exn.capture { stopped_data.foreach(stop_data(event, _)) },
      Exn.capture { super.stopping(event) }))
  }
}
