package de.sciss.lucre.experiment

trait Handle[-T, A] {
  def apply()(implicit tx: T): A
}
