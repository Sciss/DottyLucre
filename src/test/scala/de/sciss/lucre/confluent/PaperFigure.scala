package de.sciss.lucre.confluent

import java.io.File

import de.sciss.lucre.impl.MutableImpl
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Confluent, Mutable, Txn => LTxn, Var => LVar}
import de.sciss.serial.{DataInput, DataOutput, WritableFormat}

class Nodes[T <: LTxn[T]] {
  object Node {
    implicit object ser extends WritableFormat[T, Node] {
      def readT(in: DataInput)(implicit tx: T): Node = new Node with MutableImpl[T] {
        val id    = tx.readId(in)
        val value = id.readVar[Int](in)
        val next  = id.readVar[Option[Node]](in)
      }
    }

    def apply(init: Int)(implicit tx: T): Node = new Node with MutableImpl[T] {
      val id    = tx.newId()
      val value = id.newVar(init)
      val next  = id.newVar(Option.empty[Node])
    }
  }
  trait Node extends Mutable[T] {
    def value : LVar[Int]
    def next  : LVar[Option[Node]]

    def disposeData(): Unit = {
      value.dispose()
      next .dispose()
    }

    def writeData(out: DataOutput): Unit = {
      value.write(out)
      next .write(out)
    }
  }
}

object PaperFigure extends App {
  val dir     = File.createTempFile("database", "db")
  dir.delete()
  val store   = BerkeleyDB.factory(dir)
  val s       = Confluent(store)

  new Example(s)
}

class Example(val s: Confluent) {
  type T = Confluent.Txn
  val nodes = new Nodes[T]
  import nodes._

  val (access, cursor) = s.cursorRoot(_ => Option.empty[Node])(implicit tx => _ => s.newCursor())

  cursor.step { implicit tx =>
    val w1    = Node(3)
    val w2    = Node(5)
    w1.next() = Some(w2)
    access()  = Some(w1)
  }
}
