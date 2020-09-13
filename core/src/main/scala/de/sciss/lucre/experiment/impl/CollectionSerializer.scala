package de.sciss.lucre.experiment
package impl

import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.serial.impl.CollectionSerializer

import scala.collection.mutable

abstract class CollectionSerializer[T <: Exec[T], A, That <: Traversable[A]] extends TSerializer[T, That] {
  def newBuilder: mutable.Builder[A, That]
  def empty     : That

  def peer      : TSerializer[T, A]

  final def write(coll: That, out: DataOutput): Unit = {
    out.writeInt(coll.size)
    val ser = peer
    coll.foreach(ser.write(_, out))
  }

  final override def read(in: DataInput, tx: T)(implicit acc: tx.Acc): That = {
    val sz = in.readInt()
    if (sz == 0) empty
    else {
      val b = newBuilder
      b.sizeHint(sz)
      val ser = peer
      var rem = sz
      while (rem > 0) {
        b += ser.read(in, tx)
        rem -= 1
      }
      b.result()
    }
  }
}

final class ListSerializer[T <: Exec[T], A](val peer: TSerializer[T, A])
  extends CollectionSerializer[T, A, List[A]] {
  def newBuilder: mutable.Builder[A, List[A]] = List.newBuilder[A]
  def empty: List[A] = Nil
}