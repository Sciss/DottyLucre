package de.sciss.lucre

object TDisposable {
  private object Empty extends TDisposable[Any] {
    override def toString = "TDisposable.empty"

    def dispose()(implicit tx: Any): Unit = ()
  }

  def empty[T]: TDisposable[T] = Empty

  def seq[T](xs: TDisposable[T]*): TDisposable[T] = new TDisposable[T] {
    def dispose()(implicit tx: T): Unit = xs.foreach(_.dispose())
  }
}
trait TDisposable[-T] {
  def dispose()(implicit tx: T): Unit
}
