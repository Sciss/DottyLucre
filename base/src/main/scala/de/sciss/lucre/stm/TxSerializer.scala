package de.sciss.lucre.stm

import de.sciss.serial.{DataInput, DataOutput}

object TxSerializer {
  implicit def immutable[T <: Txn[T], A](implicit peer: NewImmutSerializer[A]): TxSerializer[T, A] =
    peer.asInstanceOf[TxSerializer[T, A]]
}
trait TxSerializer[T <: Txn[T], A] {
  def read(in: DataInput, tx: T)(implicit acc: tx.Acc): A

  def write(v: A, out: DataOutput): Unit
}

object NewImmutSerializer {
  implicit object int extends NewImmutSerializer[Int] {
    override def read(in: DataInput): Int = in.readInt()

    override def write(v: Int, out: DataOutput): Unit = out.writeInt(v)
  }
}
trait NewImmutSerializer[A] extends TxSerializer[AnyTxn, A] {
  override def read(in: DataInput, tx: AnyTxn)(implicit acc: tx.Acc): A = read(in)
  
  def read(in: DataInput): A
}