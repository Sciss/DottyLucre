/*
 *  CursorsSpec.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.confluent

import de.sciss.lucre.impl.MutableImpl
import de.sciss.lucre.{Var => LVar}
import de.sciss.serial.{DataInput, DataOutput, WritableFormat}

import scala.collection.immutable.{IndexedSeq => Vec}

/*

  To run only this test:

  testOnly de.sciss.lucre.confluent.CursorsSpec

 */
class CursorsSpec extends ConfluentSpec {
  //   confluent.showLog = true

  object Entity {
    implicit object CursorFmt extends WritableFormat[T, Cursor[T, D]] {
      override def readT(in: DataInput)(implicit tx: T): Cursor[T, D] = {
        tx.system.readCursor(in)
      }
    }

    implicit object Fmt extends WritableFormat[T, Entity] {
      override def readT(in: DataInput)(implicit tx: T): Entity = {
        val id      = tx.readId(in)
        val field   = id.readIntVar(in)
        val cursors = id.readVar[Vec[Cursor[T, D]]](in)
        new Entity(id, field, cursors)
      }
    }

    def apply(init: Int)(implicit tx: T): Entity = {
      val id = tx.newId()
      val field = id.newIntVar(init)
      //         val dtx: D#Tx  = tx.durable
      //         val did        = dtx.newId()
      val initCsr = Vec(tx.system.newCursor(tx.inputAccess), tx.system.newCursor(tx.inputAccess))
      //         val cursors    = dtx.newVar[ Vec[ Cursor[ S ]]]( did, initCsr )
      val cursors = id.newVar[Vec[Cursor[T, D]]](initCsr)
      new Entity(id, field, cursors)
    }
  }

  class Entity(val id: Ident[T], val field: LVar[T, Int], cursorsVar: LVar[T, Vec[Cursor[T, D]]])
    extends MutableImpl[T] {

    protected def disposeData()(implicit tx: T): Unit = {
      field     .dispose()
      cursorsVar.dispose()
    }

    def cursors(implicit tx: T): Vec[Cursor[T, D]] = {
      cursorsVar()
    }

    protected def writeData(out: DataOutput): Unit = {
      field     .write(out)
      cursorsVar.write(out)
    }
  }

  "Multiple cursors" should "work independently" in { system =>
    val (access, cursors) = system.cursorRoot { implicit tx =>
      Entity( 0 )
    } { implicit tx => _.cursors }

    val zipped = cursors.zipWithIndex

    zipped.foreach { case (cursor, idx) =>
      cursor.step { implicit tx =>
        val e = access()
        e.field() = idx + 1
      }
    }

    val res = cursors.map { cursor =>
      cursor.step { implicit tx =>
        val e = access()
        e.field()
      }
    }

    assert(res === Vec(1, 2))
  }
}
