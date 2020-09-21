package de.sciss.lucre.confluent

import de.sciss.lucre

trait TSource[T <: Txn[T], +A] extends lucre.TSource[T, A] {
  def meld(from: Access[T])(implicit tx: T): A
}

trait TRef[T <: Txn[T], A] extends lucre.TRef[T, A] with TSource[T, A] {
  def meld(from: Access[T])(implicit tx: T): A
}
