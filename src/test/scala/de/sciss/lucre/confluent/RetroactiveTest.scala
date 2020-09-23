package de.sciss.lucre.confluent

import de.sciss.lucre.Confluent
import de.sciss.lucre.impl.MutableImpl
import de.sciss.lucre.{Var => LVar}
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.serial.{DataInput, DataOutput, WritableFormat}

/*
  -- output --

INIT


MK CURSOR 2


In 1 update(1)


In 2 update(1)

(A) In cursor 1: (Path(0, 1),Foo(a = 0, b = bar))
(A) In cursor 2: (Path(0, 2),Foo(a = 0, b = baz))

RETRO

Retro input access was Path(0, 0)
(B) In cursor 1: (Path(0, 1),Foo(a = 0, b = bar))
(B) In cursor 2: (Path(0, 2),Foo(a = 0, b = baz))
(C) In cursor 1: (Path(0, 1),Foo(a = 0, b = bar))
(C) In cursor 2: (Path(0, 4),Foo(a = 666, b = baz))


 */

// XXX TODO make this a ScalaTest spec
object RetroactiveTest extends App {
  val system    = Confluent(BerkeleyDB.tmp())
  type S        = Confluent
  type T        = Confluent.Txn

  //  showLog       = true
  //  showCursorLog = true

  implicit object Fmt extends WritableFormat[T, Foo] {
    def readT(in: DataInput)(implicit tx: T): Foo = {
      val id = tx.readId(in)
      new Foo(id, id.readIntVar(in), id.readVar[String](in))
    }
  }

  final class Foo(val id: Ident[T], val a: LVar[Int], val b: LVar[String]) extends MutableImpl[T] {
    def writeData(out: DataOutput): Unit = {
      a.write(out)
      b.write(out)
    }

    def disposeData(): Unit = {
      a.dispose()
      b.dispose()
    }

    def print: String = s"Foo(a = ${a()}, b = ${b()})"
  }

  println("\nINIT\n")

  val (access, (cursor0, cursor1)) = system.cursorRoot { implicit tx =>
    val id = tx.newId()
    new Foo(id, id.newIntVar(0), id.newVar("foo"))
  } { implicit tx => _ =>
    system.newCursor() -> system.newCursor()
  }

  println("\nMK CURSOR 2\n")

  val cursor2 = cursor1.step { implicit tx => system.newCursor() }

  println("\nIn 1 update(1)\n")

  cursor1.step { implicit tx =>
    access().b() = "bar"
  }

  println("\nIn 2 update(1)\n")

  cursor2.step { implicit tx =>
    access().b() = "baz"
  }

  println(s"(A) In cursor 1: ${cursor1.step { implicit tx => tx.inputAccess -> access().print }}")
  println(s"(A) In cursor 2: ${cursor2.step { implicit tx => tx.inputAccess -> access().print }}")

  println("\nRETRO\n")

  val retroAcc = cursor0.stepFrom(Access.root, retroactive = true) { implicit tx =>
    access().a() = 666
    tx.inputAccess
  }
  println(s"Retro input access was $retroAcc")

  println(s"(B) In cursor 1: ${cursor1.step { implicit tx => tx.inputAccess -> access().print }}")
  println(s"(B) In cursor 2: ${cursor2.step { implicit tx => tx.inputAccess -> access().print }}")

  cursor2.step { implicit tx =>
    val id = tx.newId()
    id.newIntVar(666)   // enforce write version
  }

  println(s"(C) In cursor 1: ${cursor1.step { implicit tx => tx.inputAccess -> access().print }}")
  println(s"(C) In cursor 2: ${cursor2.step { implicit tx => tx.inputAccess -> access().print }}")

  //  println()
  //
  //  println(cursor2.step { implicit tx =>
  //    val idx = tx.inputAccess.index
  //    system.debugPrintIndex(idx)
  //  })
}