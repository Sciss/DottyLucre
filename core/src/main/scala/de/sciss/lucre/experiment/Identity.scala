package de.sciss.lucre.experiment

import de.sciss.serial.DataInput

object Identity {
  trait Var[T <: Txn[T], A] {
    def apply(): A
    def update(value: A): Unit
  }
  
  trait Ident[T <: Txn[T]] {
    def newVar[A](init: A): Var[T, A]
  }
  
  trait Txn[T <: Txn[T]] {
    self: T =>
   
    type Acc
    
    def newId(): Ident[T]
    
    def readId(in: DataInput)(implicit acc: Acc): Ident[T]
  }
}
