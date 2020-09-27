/*
 *  RefreshSpec.scala
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
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Confluent, Cursor => LCursor, Var => LVar}
import de.sciss.serial.{DataInput, DataOutput, WritableFormat}
import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec
import org.scalatest.matchers.should.Matchers

/*

  To run only this test:

  testOnly de.sciss.lucre.confluent.RefreshSpec

 */
class RefreshSpec extends FixtureAnyFlatSpec with Matchers {
  type S            = Confluent
  type T            = Confluent.Txn
  type FixtureParam = LCursor[T]

  // confluent.showLog = true

  object Entity {
    implicit object Fmt extends WritableFormat[T, Entity] {
      override def readT(in: DataInput)(implicit tx: T): Entity = {
        val id    = tx.readId(in)
        val field = id.readIntVar(in)
        new Entity(id, field)
      }
    }

    def apply(init: Int)(implicit tx: T): Entity = {
      val id    = tx.newId()
      val field = id.newIntVar(init)
      new Entity(id, field)
    }
  }

  class Entity(val id: Ident[T], val field: LVar[T, Int]) extends MutableImpl[T] {
    protected def disposeData()(implicit tx: T): Unit = field.dispose()

    protected def writeData(out: DataOutput): Unit = field.write(out)
  }

  def withFixture(test: OneArgTest): Outcome = {
    val system = Confluent(BerkeleyDB.tmp())
    try {
      val (_, cursor) = system.cursorRoot(_ => ())(implicit tx => _ => system.newCursor())
      test(cursor)
    }
    finally {
      system.close()
    }
  }

  "An entity" should "serialize and deserialize via tx.refresh" in { cursor =>
    val value = 1234
    val h = cursor.step { implicit tx =>
      val ent = Entity(value)
      tx.newHandle(ent)
    }
    val res = cursor.step { implicit tx =>
      val ent = h() // tx.refresh( csrStale, entStale )
      ent.field()
    }
    assert(res == value, res)
  }
}
