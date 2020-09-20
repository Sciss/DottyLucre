package de.sciss.lucre

trait TSource[-T, +A] {
  def apply()(implicit tx: T): A
}

trait TSink[-T, -A] {
  def update(value: A)(implicit tx: T): Unit
}

trait TVar[-T, A] extends TSource[T, A] with TSink[T, A]
