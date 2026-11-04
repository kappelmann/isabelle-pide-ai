/*  Title:      AI/src/General/lifecycle_hooks.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

trait Lifecycle_Hooks[A, B] {
  def started(arg: A): Unit = ()
  // called before each normal stop or after an abrupt stop
  // note: started might not have been finished or called yet
  def stopping(arg: B): Unit = ()
}
