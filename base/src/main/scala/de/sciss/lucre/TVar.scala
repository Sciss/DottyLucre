package de.sciss.lucre

import de.sciss.serial.Writable

trait TSource[-T, +A] {
  def apply()(implicit tx: T): A
}

trait TSink[-T, -A] {
  def update(value: A)(implicit tx: T): Unit
}

trait TRef[-T, A] extends TSource[T, A] with TSink[T, A]

trait TVar[-T, A] extends TRef[T, A] with Writable with TDisposable[T]
