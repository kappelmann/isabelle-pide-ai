/*  Title:      AI/src/Services/node_cache_service.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object Node_Cache_Service {
  def delay(options: Options): Time = options.seconds("pide_ai_node_cache_delay")

  type Data = (() => Unit, Throttle[Unit], PIDE_AI_Progress)
}

class Node_Cache_Service extends PIDE_AI_Service("node_cache")
with Session_Data_Service[Node_Cache_Service.Data] {
  def description: String = "Refresh cached data of changed nodes."

  def start_data(event: PIDE_AI_Service.Session_Event): Node_Cache_Service.Data = {
    val session = event.session
    val progress = new Sub_Progress(event.server.base_progress)
    val refresh = new Throttle[Unit](Node_Cache_Service.delay(session.session_options),
      event.server.log)(_ => session.node_cache.refresh(session.snapshot(), progress))
    val listener = () => refresh(())
    session.node_cache += listener
    (listener, refresh, progress)
  }

  def stop_data(event: PIDE_AI_Service.Session_Event, data: Node_Cache_Service.Data): Unit = {
    val (listener, refresh, progress) = data
    PIDE_AI_Util.release_combined(List(
      Exn.capture { event.session.node_cache -= listener },
      Exn.capture { refresh.stop() },
      Exn.capture { progress.stop() }))
  }
}

class Node_Cache_Services extends PIDE_AI_Services(new Node_Cache_Service)
