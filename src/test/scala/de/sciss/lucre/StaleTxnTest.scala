/*
 *  StaleTxnTest.scala
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

package de.sciss.lucre

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.store.BerkeleyDB

// XXX TODO turn this into a ScalaTest spec
object StaleTxnTest {
  def main(args: Array[String]): Unit = {
//    type S = InMemory
//    type T = InMemory.Txn
//    val system = InMemory()

    val store  = BerkeleyDB.tmp()
    val system = Durable(store)

    run(system)

    system.close()
  }

  def run[T <: Txn[T]](cursor: Cursor[T]): Unit = {
    val lH = cursor.step { implicit tx =>
      val l = SkipList.Set.empty[T, Int]()
      l.add(12)
      l.add(34)
      implicit val fmt = SkipList.Set.format[T, Int]()
      tx.newHandle(l)
    }

    val done = new AnyRef

    val t = new Thread {
      override def run(): Unit = {
        val res = cursor.step { implicit tx =>
          val l = lH()
          // why this is not causing an error - unless we run on a different thread:
          // we are in the same thread as before, and therefore
          // the InTxnImpl is reused, it goes back from
          // `Detached` to `Active`.
          l.add(78)
          (l.contains(12), l.contains(56))
        }

        println(s"Result: $res")

        done.synchronized(done.notifyAll())
      }
    }

    t.start(); done.synchronized(done.wait())
  }
}
