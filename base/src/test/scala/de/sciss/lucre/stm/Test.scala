package de.sciss.lucre

object Test {
  def apply[Tx <: Exec[Tx]]()(implicit tx: Tx): Test[Tx] = new Impl(tx)
  
  private class Impl[Tx <: Exec[Tx]](tx0: Tx) extends Test[Tx] {
    private val _id = tx0.newId()
    private val vr  = _id.newIntVar(0)(tx0)
    
    def id: Ident[Tx] = _id
    
    def apply()(implicit tx: Tx): Int = vr()

    def update(value: Int)(implicit tx: Tx): Unit = 
      vr() = value
  }
}
trait Test[Tx <: Exec[Tx]] {
  def id: Ident[Tx]
  
  def apply()(implicit tx: Tx): Int
  
  def update(value: Int)(implicit tx: Tx): Unit
}
