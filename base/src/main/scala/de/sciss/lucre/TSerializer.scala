package de.sciss.lucre

import de.sciss.lucre.impl.ListSerializer
import de.sciss.lucre.{AnyExec, Exec}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}

object TSerializer {
  implicit def immutable[T <: Exec[T], A](implicit peer: NewImmutSerializer[A]): TSerializer[T, A] =
    peer.asInstanceOf[TSerializer[T, A]]

  implicit def list[T <: Exec[T], A](implicit peer: TSerializer[T, A]): TSerializer[T, List[A]] =
    new ListSerializer[T, A](peer)
}
trait TSerializer[T <: Exec[T], A] {
  def read(in: DataInput, tx: T)(implicit acc: tx.Acc): A

  def write(v: A, out: DataOutput): Unit
}

object NewImmutSerializer {
  implicit object Int extends NewImmutSerializer[scala.Int] {
    override def read(in: DataInput): scala.Int = in.readInt()

    override def write(v: Int, out: DataOutput): Unit = out.writeInt(v)
  }

  implicit object Long extends NewImmutSerializer[scala.Long] {
    def read(in: DataInput): scala.Long = in.readLong()

    def write(v: scala.Long, out: DataOutput): Unit = out.writeLong(v)
  }

  implicit object String extends NewImmutSerializer[java.lang.String] {
    def read(in: DataInput): java.lang.String = in.readUTF()

    def write(v: java.lang.String, out: DataOutput): Unit = out.writeUTF(v)
  }
}
trait NewImmutSerializer[A] extends TSerializer[AnyExec, A] {
  override def read(in: DataInput, tx: AnyExec)(implicit acc: tx.Acc): A = read(in)

  def read(in: DataInput): A
}