package de.sciss.lucre.stm

object Test {
  def apply[Tx <: Txn[Tx]]()(implicit tx: Tx): Test[Tx] = new Impl(tx)
  
  private class Impl[Tx <: Txn[Tx]](tx0: Tx) extends Test[Tx] {
    private val _id = tx0.newId()
    private val vr  = tx0.newIntVar(_id, 0)
    
//    def id: Tx#Id = _id
    
    def id: Identifier[Tx] = _id
    
    def apply()(implicit tx: Tx): Int = vr()

    def update(value: Int)(implicit tx: Tx): Unit = 
      vr() = value
  }
}
trait Test[Tx <: Txn[Tx]] {
//  def id: Tx#Id
  def id: Identifier[Tx]
  
  def apply()(implicit tx: Tx): Int
  
  def update(value: Int)(implicit tx: Tx): Unit
}
