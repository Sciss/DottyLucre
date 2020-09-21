package de.sciss.lucre.confluent

import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Confluent, Durable}
import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec
import org.scalatest.matchers.should.Matchers

// helper trait providing a fixture
trait ConfluentSpec extends FixtureAnyFlatSpec with Matchers {
  final type S = Confluent
  final type T = Confluent.Txn
  final type D = Durable.Txn
  final type FixtureParam = Confluent // confluent.Cursor[ S ]

  final def withFixture(test: OneArgTest): Outcome = {
    val system = Confluent(BerkeleyDB.tmp())
    try {
      //         val (_, cursor) = system.cursorRoot( _ => () )( tx => _ => tx.newCursor() )
      //         test( cursor )
      test(system)
    }
    finally {
      system.close()
    }
  }
}