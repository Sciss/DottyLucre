package de.sciss.lucre.confluent

import de.sciss.lucre

trait Source[T <: Txn[T], +A] extends lucre.Source[T, A] {
  def meld(from: Access[T])(implicit tx: T): A
}

trait Ref[T <: Txn[T], A] extends lucre.Ref[T, A] with Source[T, A] {
  def meld(from: Access[T])(implicit tx: T): A
}
