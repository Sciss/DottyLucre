package de.sciss.lucre.experiment.test

import de.sciss.lucre.experiment.{Handle, Ident, IdentMap, Obj, Txn}

trait TestIdent[T <: Txn[T]] {
  val tx0: T

  private val map: IdentMap[Ident[T] /*tx0.Id*/, T, Handle[T, Obj[T]]] = tx0.newIdentMap[Handle[T, Obj[T]]]

  def foo(in: Obj[T])(implicit tx: T): Unit = {
    val inH = tx.newHandle(in)
     map.put(in.id /*.!*/, inH)
  }
}
