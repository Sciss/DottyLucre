package de.sciss.lucre

object Disposable {
  private object Empty extends Disposable[Any] {
    override def toString = "TDisposable.empty"

    def dispose()(implicit tx: Any): Unit = ()
  }

  def empty[T]: Disposable[T] = Empty

  def seq[T](xs: Disposable[T]*): Disposable[T] = new Disposable[T] {
    def dispose()(implicit tx: T): Unit = xs.foreach(_.dispose())
  }
}
trait Disposable[-T] {
  def dispose()(implicit tx: T): Unit
}
