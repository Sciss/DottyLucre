package de.sciss.lucre.impl

import de.sciss.lucre.TSerializer
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable

abstract class CollectionSerializer[-T, A, That <: Traversable[A]] extends TSerializer[T, That] {
  protected def newBuilder: mutable.Builder[A, That]
  protected def empty     : That

  def peer: TSerializer[T, A]

  final def write(coll: That, out: DataOutput): Unit = {
    out.writeInt(coll.size)
    val ser = peer
    coll.foreach(ser.write(_, out))
  }

  final override def readT(in: DataInput)(implicit tx: T): That = {
    val sz = in.readInt()
    if (sz == 0) empty
    else {
      val b = newBuilder
      b.sizeHint(sz)
      val ser = peer
      var rem = sz
      while (rem > 0) {
        b += ser.readT(in)
        rem -= 1
      }
      b.result()
    }
  }
}

final class ListSerializer[-T, A](val peer: TSerializer[T, A])
  extends CollectionSerializer[T, A, List[A]] {

  protected def newBuilder: mutable.Builder[A, List[A]] = List.newBuilder[A]

  protected def empty: List[A] = Nil
}

final class VecSerializer[-T, A](val peer: TSerializer[T, A])
  extends CollectionSerializer[T, A, Vec[A]] {

  protected def newBuilder: mutable.Builder[A, Vec[A]] = Vector.newBuilder[A]

  protected def empty: Vec[A] = Vector.empty
}
