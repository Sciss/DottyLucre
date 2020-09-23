package de.sciss.lucre.confluent

import de.sciss.lucre.impl.MutableImpl
import de.sciss.lucre.{ConfluentLike, Mutable, Var => LVar}
import de.sciss.serial.{DataInput, DataOutput, WritableFormat}

trait TestHasLinkedList {
  class Types[T <: Txn[T]](val s: ConfluentLike[T]) {
    type Sys = T

    object Node {
      implicit object ser extends WritableFormat[T, Node] {
        override def readT(in: DataInput)(implicit tx: T): Node = {
          val id = tx.readId(in)
          readData(in, id)
        }

        private def readData(in: DataInput, _id: Ident[T]): Node = new Node with MutableImpl[T] {
          val id    : Ident[T]              = _id
          val name  : String                = in.readUTF()
          val value : LVar[T, Int]          = id.readIntVar(in)
          val next  : LVar[T, Option[Node]] = id.readVar[Option[Node]](in)
        }
      }

      def apply(_name: String, init: Int)(implicit tx: T): Node = new Node with MutableImpl[T] {
        val id    : Ident[T]              = tx.newId()
        val name  : String                = _name
        val value : LVar[T, Int]          = id.newIntVar(init)
        val next  : LVar[T, Option[Node]] = id.newVar[Option[Node]](None)
      }
    }

    trait Node extends Mutable[T] {
      def name  : String
      def value : LVar[T, Int]
      def next  : LVar[T, Option[Node]]

      protected def disposeData()(implicit tx: T): Unit = {
        value.dispose()
        next .dispose()
      }

      protected def writeData(out: DataOutput): Unit = {
        out.writeUTF(name)
        value.write(out)
        next .write(out)
      }

      override def toString = s"Node($name, $id)"
    }

    def toList(next: Option[Node])(implicit tx: T): List[(String, Int)] = next match {
      case Some(n)  => (n.name, n.value()) :: toList(n.next())
      case _        => Nil
    }

    // tuples of (name, value, id.path)
    def toListId(next: Option[Node])(implicit tx: T): List[(String, Int, String)] = next match {
      case Some(n)  => (n.name, n.value(), n.id.!.path.toString) :: toListId(n.next())
      case _        => Nil
    }
  }
}