/*  Title:      AI/src/PIDE/pide_ai_node_cache.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Node_Cache {
  final class Data[A](val make: Document.Snapshot => A)

  sealed case class Entry(
    serial: Counter.ID = 0,
    node: Option[Document.Node] = None,
    values: Map[Data[?], Any] = Map.empty
  ) {
    def get[A](data: Data[A], node: Document.Node): Option[A] =
      if (this.node.exists(_ eq node)) values.get(data).map(_.asInstanceOf[A]) else None

    def put[A](data: Data[A], node: Document.Node, value: A): Entry =
      if (this.node.exists(_ eq node)) copy(values = values + (data -> value))
      else Entry(serial, Some(node), Map(data -> value))
  }

  sealed case class State(
    entries: Map[Document.Node.Name, Entry] = Map.empty,
    requested: Set[Data[?]] = Set.empty
  ) {
    def entry(name: Document.Node.Name): Entry = entries.getOrElse(name, Entry())
  }

  final class View private[PIDE_AI_Node_Cache](
    cache: PIDE_AI_Node_Cache,
    val snapshot: Document.Snapshot,
    private var state: State
  ) {
    def apply[A](data: Data[A], name: Document.Node.Name): A = {
      val node = snapshot.version.nodes(name)
      val entry = state.entry(name)
      entry.get(data, node).getOrElse {
        val value = data.make(snapshot.switch(name))
        cache.store(name, entry.serial, data, node, value)
        state = state.copy(entries = state.entries + (name -> entry.put(data, node, value)))
        value
      }
    }
  }
}

class PIDE_AI_Node_Cache {
  private val state = Synchronized(PIDE_AI_Node_Cache.State())
  private val counter = Counter.make()
  private val listeners = Synchronized(List.empty[() => Unit])

  def += (listener: () => Unit): Unit = listeners.change(Library.update(listener))
  def -= (listener: () => Unit): Unit = listeners.change(Library.remove(listener))

  def changed(names: Iterable[Document.Node.Name]): Unit =
    if (names.nonEmpty) {
      state.change(st => st.copy(entries =
        st.entries ++ names.map(_ -> PIDE_AI_Node_Cache.Entry(counter()))))
      listeners.value.foreach(_())
    }

  def view(snapshot: => Document.Snapshot): PIDE_AI_Node_Cache.View = {
    val st = state.value
    new PIDE_AI_Node_Cache.View(this, snapshot, st)
  }

  def refresh(snapshot: => Document.Snapshot, progress: PIDE_AI_Progress): Unit = {
    val st = state.value
    val view = new PIDE_AI_Node_Cache.View(this, snapshot, st)
    st.entries.keysIterator.flatMap(name => st.requested.iterator.map(_ -> name))
      .takeWhile(_ => !progress.stopped).foreach(view(_, _))
  }

  private def store[A](
    name: Document.Node.Name,
    serial: Counter.ID,
    data: PIDE_AI_Node_Cache.Data[A],
    node: Document.Node,
    value: A
  ): Unit =
    state.change { st =>
      val entry = st.entry(name)
      st.copy(
        entries =
          if (entry.serial == serial) st.entries + (name -> entry.put(data, node, value))
          else st.entries,
        requested = st.requested + data)
    }
}
