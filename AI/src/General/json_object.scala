/*  Title:      AI/src/General/json_object.scala
    Author:     Kevin Kappelmann

JSON objects with stable entry order.
*/

package isabelle.pide.ai

import isabelle._

import scala.collection.immutable.VectorMap

object JSON_Object {
  // stable order of entries
  def apply(entries: JSON.Object.Entry*): JSON.Object.T = VectorMap.from(entries)
  def apply(entries: IterableOnce[JSON.Object.Entry]): JSON.Object.T = VectorMap.from(entries)

  def flatten(entries: Option[JSON.Object.Entry]*): JSON.Object.T = flatten(entries)
  def flatten(entries: IterableOnce[Option[JSON.Object.Entry]]): JSON.Object.T =
    apply(entries.iterator.flatten)

  def if_proper(b: Boolean, body: => JSON.Object.T): JSON.Object.T =
    if (b) body else JSON.Object.empty

  def text(json: JSON.T): String = JSON.Value.String.unapply(json).getOrElse(JSON.Format(json))
}
