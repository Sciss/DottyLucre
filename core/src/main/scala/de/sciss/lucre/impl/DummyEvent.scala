package de.sciss.lucre
package impl

object DummyEvent {
  /** This method is cheap. */
  def apply[T <: Txn[T], A]: DummyEvent[T, A] = anyDummy.asInstanceOf[DummyEvent[T, A]]

  private val anyDummy = new Impl[AnyTxn]

  private final class Impl[T <: Txn[T]] extends DummyEvent[T, Any] {
    override def toString = "event.Dummy"
  }

  private def opNotSupported = sys.error("Operation not supported")
}

trait DummyEvent[T <: Txn[T], +A] extends EventLike[T, A] {
  import DummyEvent._

  final def ---> (sink: Event[T, Any]): Unit = ()
  final def -/-> (sink: Event[T, Any]): Unit = ()

  final def react(fun: T => A => Unit)(implicit tx: T): TDisposable[T] = TDisposable.empty[T]

  private[lucre] final def pullUpdate(pull: Pull[T]): Option[A] = opNotSupported
}