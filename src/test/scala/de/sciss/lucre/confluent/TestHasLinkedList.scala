package de.sciss.lucre.confluent

import de.sciss.lucre.impl.MutableImpl
import de.sciss.lucre.{ConfluentLike, Mutable, TSerializer, Var => LVar}
import de.sciss.serial.{DataInput, DataOutput}

trait TestHasLinkedList {
  class Types[T <: Txn[T]](val s: ConfluentLike[T]) {
    type Sys = T

    object Node {
      implicit object ser extends TSerializer[T, Node] {

        def write(v: Node, out: DataOutput): Unit = v.write(out)

        def read(in: DataInput, tx: T)(implicit access: tx.Acc): Node = {
          val id = tx.readId(in)
          readData(in, id)(tx)
        }

        private def readData(in: DataInput, _id: T#Id)(implicit tx: T): Node = new Node with MutableImpl[T] {
          val id    : Ident[T]            = _id
          val name  : String              = in.readUTF()
          val value : LVar[Int]           = id.readIntVar(in)
          val next  : LVar[Option[Node]]  = id.readVar[Option[Node]](in)
        }
      }

      def apply(_name: String, init: Int)(implicit tx: T): Node = new Node with MutableImpl[T] {
        val id    : Ident[T]            = tx.newId()
        val name  : String              = _name
        val value : LVar[Int]           = id.newIntVar(init)
        val next  : LVar[Option[Node]]  = id.newVar[Option[Node]](None)
      }
    }

    trait Node extends Mutable[T] {
      def name  : String
      def value : LVar[Int]
      def next  : LVar[Option[Node]]

      protected def disposeData(): Unit = {
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

    def toList(next: Option[Node]): List[(String, Int)] = next match {
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