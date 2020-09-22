package de.sciss.lucre

import de.sciss.lucre.impl.{ListSerializer, OptionSerializer, VecSerializer}
import de.sciss.serial.{DataInput, DataOutput, Writable}

import scala.collection.immutable.{IndexedSeq => Vec}

trait Reader[+A] {
  def read(in: DataInput): A
}

trait Writer[-A] {
  def write(value: A, out: DataOutput): Unit
}

trait TReader[-T, +A] {
  def readT(in: DataInput)(implicit tx: T): A
}

trait ConstantReader[+A] extends TReader[Any, A] with Reader[A] {
  final def readT(in: DataInput)(implicit tx: Any): A = read(in)
}

object TSerializer {
  implicit object Unit extends ConstantSerializer[scala.Unit] {
    def write(v: scala.Unit, out: DataOutput): Unit = ()

    def read(in: DataInput): scala.Unit = ()
  }

  implicit final object Boolean extends ConstantSerializer[scala.Boolean] {
    def write(v: scala.Boolean, out: DataOutput): Unit = out.writeBoolean(v)

    def read(in: DataInput): scala.Boolean = in.readBoolean()
  }

  implicit final object Char extends ConstantSerializer[scala.Char] {
    def write(v: scala.Char, out: DataOutput): Unit = out.writeChar(v.toInt)

    def read(in: DataInput): scala.Char = in.readChar()
  }

  implicit object Int extends ConstantSerializer[scala.Int] {
    override def read(in: DataInput): scala.Int = in.readInt()

    override def write(v: Int, out: DataOutput): Unit = out.writeInt(v)
  }

  implicit final object Float extends ConstantSerializer[scala.Float] {
    def write(v: scala.Float, out: DataOutput): Unit = out.writeFloat(v)

    def read(in: DataInput): scala.Float = in.readFloat()
  }

  implicit object Long extends ConstantSerializer[scala.Long] {
    def read(in: DataInput): scala.Long = in.readLong()

    def write(v: scala.Long, out: DataOutput): Unit = out.writeLong(v)
  }

  implicit final object Double extends ConstantSerializer[scala.Double] {
    def write(v: scala.Double, out: DataOutput): Unit = out.writeDouble(v)

    def read(in: DataInput): scala.Double = in.readDouble()
  }

  implicit object String extends ConstantSerializer[java.lang.String] {
    def read(in: DataInput): java.lang.String = in.readUTF()

    def write(v: java.lang.String, out: DataOutput): Unit = out.writeUTF(v)
  }

  implicit final object File extends ConstantSerializer[java.io.File] {
    def write(v: java.io.File, out: DataOutput): Unit = out.writeUTF(v.getPath)

    def read(in: DataInput): java.io.File = new java.io.File(in.readUTF())
  }

  implicit def list[T, A](implicit peer: TSerializer[T, A]): TSerializer[T, List[A]] =
    new ListSerializer[T, A](peer)

  implicit def vec[T, A](implicit peer: TSerializer[T, A]): TSerializer[T, Vec[A]] =
    new VecSerializer[T, A](peer)

  implicit def option[T, A](implicit peer: TSerializer[T, A]): TSerializer[T, Option[A]] =
    new OptionSerializer[T, A](peer)
}
trait TSerializer[-T, A] extends TReader[T, A] with Writer[A]

trait WritableSerializer[-T, A <: Writable] extends TSerializer[T, A] {
  final def write(value: A, out: DataOutput): Unit = value.write(out)
}

trait ConstantSerializer[A] extends TSerializer[Any, A] with ConstantReader[A]