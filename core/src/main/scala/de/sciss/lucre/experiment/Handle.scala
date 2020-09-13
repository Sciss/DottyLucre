package de.sciss.lucre.experiment

trait Handle[T <: Exec[T], A] {
  def apply()(implicit tx: T): A
}
