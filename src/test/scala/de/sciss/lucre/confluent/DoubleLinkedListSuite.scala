package de.sciss.lucre.confluent

import java.io.File

import de.sciss.lucre.impl.MutableImpl
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Confluent, Durable, Ident, Mutable, TSerializer, WritableSerializer, Var => LVar}
import de.sciss.serial.{DataInput, DataOutput}
import org.scalatest.GivenWhenThen
import org.scalatest.funspec.AnyFunSpec

import scala.annotation.tailrec

/*

  To run only this test:

  testOnly de.sciss.lucre.confluent.DoubleLinkedListSuite

 */
class  DoubleLinkedListSuite extends AnyFunSpec with GivenWhenThen {
  type S = Confluent
  type T = Confluent.Txn
  type D = Durable  .Txn

  describe("A Confluently Persistent Double Linked List") {
    val dir = File.createTempFile("database", "db")
    dir.delete()
    val store = BerkeleyDB.factory(dir)
    val _s = Confluent(store)
    val types = new Types(_s)

    import types._

    it("should be possible to navigate forward and backward and do updates") {

      ///////////////////////////// v0 /////////////////////////////

      Given("v0 : Allocate node w0, with x = 1")
      implicit val whyOhWhy: TSerializer[T, Node] = Node.ser
      val (access, cursor) = s.cursorRoot[Option[Node], Cursor[T, D]] { implicit tx =>
        val w0 = Node("w0", 1)
        Some(w0)
      } { implicit tx => _ => tx.system.newCursor() }

      ///////////////////////////// v1 /////////////////////////////

      Given("v1 : Append a new node w1 with x = 2")
      cursor.step { implicit tx =>
        val head = access()
        val newLast = Node("w1", 2)
        @tailrec def step(last: Node): Unit =
          last.next() match {
            case None =>
              last   .next() = Some(newLast)
              newLast.prev() = Some(last)
            case Some(n1) => step(n1)
          }

        head match {
          case Some(n)  => step(n)
          case None     => access() = Some(newLast)
        }
      }

      When("the result is converted to a plain list in a new transaction")
      val (_, res1) = cursor.step { implicit tx =>
        val node = access()
        tx.inputAccess -> toList(node)
      }

      val exp1 = List("w0" -> 1, "w1" -> 2)
      Then(s"is should equal $exp1")
      assert(res1 === exp1)

      ///////////////////////////// v2 /////////////////////////////

      Given("v2 : Increment all nodes by 2")
      //         timeWarp( Confluent.Path.root )
      cursor.step { implicit tx =>
        @tailrec def step(last: Option[Node]): Unit =
          last match {
            case None =>
            case Some(n) =>
              n.value() = n.value() + 2 // n.value.transform(_ + 2)
              step(n.next())
          }

        step(access())
      }

      When("the result is converted to a plain list in a new transaction")
      val (_, res2) = cursor.step { implicit tx =>
        val node = access()
        tx.inputAccess -> toList(node)
      }

      val exp2 = List("w0" -> 3, "w1" -> 4)
      Then(s"is should equal $exp2")
      assert(res2 === exp2)

      When("the result is converted to a plain list going from back to front")
      val res2b = cursor.step { implicit tx =>
        @tailrec def findLast(n: Node): Node = n.next() match {
          case None     => n
          case Some(n1) => findLast(n1)
        }
        @tailrec def reverseToList(n: Node, res: List[(String, Int)]): List[(String, Int)] = {
          val res1 = (n.name -> n.value()) :: res
          n.prev() match {
            case None     => res1
            case Some(n1) => reverseToList(n1, res1)
          }
        }

        access() match {
          case Some(n) =>
            reverseToList(findLast(n), Nil)

          case None => Nil
        }
      }

      Then("is should have the same result")
      assert(res2b === exp2)

      ///////////////////////////// v3 /////////////////////////////

      Given("v3 : Increment all nodes by 2, going from back to front")
      //         timeWarp( Confluent.Path.root )
      cursor.step { implicit tx =>
        @tailrec def findLast(n: Node): Node = n.next() match {
          case None     => n
          case Some(n1) => findLast(n1)
        }
        @tailrec def step(n: Node): Unit = {
          n.value() = n.value() + 2 // n.value.transform(_ + 2)
          n.prev() match {
            case None     =>
            case Some(n1) => step(n1)
          }
        }
        access() match {
          case Some(n)  => step(findLast(n))
          case None     =>
        }
      }

      When("the result is converted to a plain list in a new transaction")
      val (_, res3) = cursor.step { implicit tx =>
        val node = access()
        tx.inputAccess -> toList(node)
      }

      val exp3 = List("w0" -> 5, "w1" -> 6)
      Then(s"is should equal $exp3")
      assert(res3 === exp3)
    }
  }

  class Types(val s: S) {
    object Node {
      implicit object ser extends WritableSerializer[T, Node] {
        override def readT(in: DataInput)(implicit tx: T): Node = {
          val id = tx.readId(in)
          readData(in, id)(tx)
        }

        private def readData(in: DataInput, _id: Ident[T])(implicit tx: T): Node = new Node with MutableImpl[T] {
          val id    : Ident[T]            = _id
          val name  : String              = in.readUTF()
          val value : LVar[Int]           = id.readIntVar(in)
          val prev  : LVar[Option[Node]]  = id.readVar[Option[Node]](in)
          val next  : LVar[Option[Node]]  = id.readVar[Option[Node]](in)
        }
      }

      def apply(_name: String, init: Int)(implicit tx: T): Node = new Node with MutableImpl[T] {
        val id    : Ident[T]            = tx.newId()
        val name  : String              = _name
        val value : LVar[Int]           = id.newIntVar(init)
        val prev  : LVar[Option[Node]]  = id.newVar[Option[Node]](None)
        val next  : LVar[Option[Node]]  = id.newVar[Option[Node]](None)
      }
    }

    trait Node extends Mutable[T] {
      def name: String

      def value: LVar[Int]

      def prev: LVar[Option[Node]]
      def next: LVar[Option[Node]]

      protected def disposeData(): Unit = {
        value.dispose()
        prev .dispose()
        next .dispose()
      }

      protected def writeData(out: DataOutput): Unit = {
        out.writeUTF(name)
        value.write(out)
        prev .write(out)
        next .write(out)
      }

      override def toString = s"Node($name, $id)"
    }

    def toList(next: Option[Node]): List[(String, Int)] = next match {
      case Some(n)  => (n.name, n.value()) :: toList(n.next())
      case _        => Nil
    }
  }
}