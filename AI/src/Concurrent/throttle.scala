/*  Title:      AI/src/Concurrent/throttle.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

class Throttle[A](delay: Time, log: Logger)(f: A => Unit) {
  private val state = Synchronized[(Option[A], Time)]((None, Time.zero))

  private val event = new Delay_Ops(log) {
    override def app(body: => Unit): Unit =
      Isabelle_Thread.fork(name = "pide_ai_throttle") {
        PIDE_AI_Util.log_failure(log, "Throttled event failed", Exn.capture(body))
      }
  }.first(state.value._2 + delay - Time.now()) {
    synchronized {
      state.change_result { (latest, _) => (latest, (None, Time.now())) }.foreach(f)
    }
  }

  def apply(a: A): Unit = {
    state.change { (_, last) => (Some(a), last) }
    event.invoke()
  }
}
