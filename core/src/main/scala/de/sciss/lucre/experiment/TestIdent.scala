package de.sciss.lucre.experiment

trait TestIdent[T <: Txn[T]] {
  def tx0: T
  
  private val map = tx0.newIdentMap[Handle[T, Obj[T]]]
  
  def foo(in: Obj[T])(implicit tx: T): Unit = {
    val inH = tx.newHandle(in)
    // map.put(in.id.!, inH)  // XXX TODO
  }
}
