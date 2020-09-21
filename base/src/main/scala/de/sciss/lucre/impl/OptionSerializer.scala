package de.sciss.lucre.impl

import de.sciss.lucre.{Exec, TSerializer}
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.switch

final class OptionSerializer[T <: Exec[T], A](peer: TSerializer[T, A])
  extends TSerializer[T, Option[A]] {

  def write(opt: Option[A], out: DataOutput): Unit =
    opt match {
      case Some(v)  => out.writeByte(1); peer.write(v, out)
      case _        => out.writeByte(0)
    }

  def read(in: DataInput, tx: T)(implicit access: tx.Acc): Option[A] = (in.readByte(): @switch) match {
    case 1 => Some(peer.read(in, tx))
    case 0 => None
  }
}
