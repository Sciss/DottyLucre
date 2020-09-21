package de.sciss.lucre

import de.sciss.lucre.impl.{ListSerializer, OptionSerializer, VecSerializer}
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}

trait TReader[T <: Exec[T], +A] {
  def read(in: DataInput, tx: T)(implicit access: tx.Acc): A
}

object TSerializer {
  // XXX TODO -- we can no longer cast, another shitty result of dropping type projections
  implicit def immutable[T <: Exec[T], A](implicit peer: NewImmutSerializer[A]): TSerializer[T, A] =
    new TSerializer[T, A] {
      def read(in: DataInput, tx: T)(implicit acc: tx.Acc): A = peer.read(in)

      def write(v: A, out: DataOutput): Unit = peer.write(v, out)
    }

  implicit def list[T <: Exec[T], A](implicit peer: TSerializer[T, A]): TSerializer[T, List[A]] =
    new ListSerializer[T, A](peer)

  implicit def vec[T <: Exec[T], A](implicit peer: TSerializer[T, A]): TSerializer[T, Vec[A]] =
    new VecSerializer[T, A](peer)

  implicit def option[T <: Exec[T], A](implicit peer: TSerializer[T, A]): TSerializer[T, Option[A]] =
    new OptionSerializer[T, A](peer)
}
trait TSerializer[T <: Exec[T], A] extends TReader[T, A] {
  def write(v: A, out: DataOutput): Unit
}

object NewImmutSerializer {
  implicit object Unit extends NewImmutSerializer[scala.Unit] {
    def write(v: scala.Unit, out: DataOutput): Unit = ()

    def read(in: DataInput): scala.Unit = ()
  }

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
trait NewImmutSerializer[A] {
//  override def read(in: DataInput, tx: AnyExec)(implicit acc: tx.Acc): A =
//    read(in)

  def read(in: DataInput): A

  def write(v: A, out: DataOutput): Unit
}