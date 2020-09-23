package de.sciss.lucre.confluent

import de.sciss.lucre.impl.MutableImpl
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Confluent, Mutable, Var => LVar}
import de.sciss.serial.{DataInput, DataOutput, TFormat, WritableFormat}

import scala.annotation.tailrec

// \ref{lst:lucre_durable_linkedlist}, \ref{lst:lucre_durable_traverse}, \ref{lst:linkedlist_init}

/*
    OUTPUT

    1
    2


 */
object ThesisFiatKaplanExample extends App {
  object LinkedList {
    implicit def listFmt[T <: Txn[T], A](implicit peerFmt: TFormat[T, A]): TFormat[T, LinkedList[T, A]] =
      new ListFmt[T, A]

    private class ListFmt[T <: Txn[T], A](implicit peer: TFormat[T, A])
      extends WritableFormat[T, LinkedList[T, A]] {

      def readT(in: DataInput)(implicit tx: T): LinkedList[T, A] = {
        new Impl[T, A] {
          val peerFmt = peer
          val id      = tx.readId(in)
          val head    = id.readVar[Option[Cell]](in)(TFormat.option(CellFmt))
        }
      }
    }

    def apply[T <: Txn[T], A]()(implicit tx: T, peer: TFormat[T, A]): LinkedList[T, A] =
      new Impl[T, A] {
        val peerFmt = peer
        val id      = tx.newId()
        val head    = id.newVar(Option.empty[Cell])
      }

    private abstract class Impl[T <: Txn[T], A] extends LinkedList[T, A] with MutableImpl[T] {
      implicit def peerFmt: TFormat[T, A]

      def cell(init: A)(implicit tx: T): Cell = new Cell {
        val next  = id.newVar(Option.empty[Cell])
        val value = init
      }

      implicit object CellFmt extends TFormat[T, Cell] {
        def write(cell: Cell, out: DataOutput): Unit = {
          cell.next.write(out)
          peerFmt.write(cell.value, out)
        }

        def readT(in: DataInput)(implicit tx: T): Cell = new Cell {
          val next  = id.readVar[Option[Cell]](in)
          val value = peerFmt.readT(in)
        }
      }

      def disposeData()(implicit tx: T): Unit = head.dispose()

      def writeData(out: DataOutput): Unit = head.write(out)
    }
  }
  trait LinkedList[T <: Txn[T], A] extends Mutable[T] { list =>

    override def toString = "LinkedList"

    def head: LVar[T, Option[Cell]]

    trait Cell {
      override def toString = s"$list.cell<$next, $value>"

      def next: LVar[T, Option[Cell]]

      def value: A
    }

    def cell(init: A)(implicit tx: T): Cell
  }

  val store = BerkeleyDB.tmp()
  val s     = Confluent(store)

  val (access, cursor) = s.cursorRoot { implicit tx =>
    val list    = LinkedList[Confluent.Txn, Int]()
    val w0      = list.cell(init = 2)
    val w1      = list.cell(init = 1)
    list.head() = Some(w0)
    w0.next()   = Some(w1)
    list
  } { implicit tx => _ => s.newCursor() }

  cursor.step { implicit tx =>
    val list      = access()
    val Some(w0)  = list.head()
    val Some(w1)  = w0.next()
    w0.next()     = None
    list.head()   = Some(w1)
    w1.next()     = Some(w0)
  }

  def traverse[T <: Txn[T], A](l: LinkedList[T, A])(implicit tx: T): Unit = {
    @tailrec
    def loop(opt: Option[l.Cell]): Unit =
      opt match {
        case Some(cell) =>
          println(cell.value)
          loop(cell.next())
        case _ =>
      }

    loop(l.head())
  }

  cursor.step { implicit tx => traverse(access()) } // 1, 2
}