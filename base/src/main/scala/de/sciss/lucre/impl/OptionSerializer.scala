package de.sciss.lucre.impl

import de.sciss.lucre.TSerializer
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.switch

final class OptionSerializer[-T, A](peer: TSerializer[T, A])
  extends TSerializer[T, Option[A]] {

  override def write(opt: Option[A], out: DataOutput): Unit =
    opt match {
      case Some(v)  => out.writeByte(1); peer.write(v, out)
      case _        => out.writeByte(0)
    }

  override def readT(in: DataInput)(implicit tx: T): Option[A] = (in.readByte(): @switch) match {
    case 1 => Some(peer.readT(in))
    case 0 => None
  }
}
