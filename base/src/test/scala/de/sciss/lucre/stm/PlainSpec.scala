package de.sciss.lucre

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PlainSpec extends AnyFlatSpec with Matchers {
  "A Plain base system" should "behave as advertised" in {
    type S = Plain    // scalac bug -- it _is_ used
    val tx = Plain.instance

    val id = tx.newId()
    val vrS = id.newVar("Hello")
    assert(vrS() === "Hello")
    vrS() = "World"
    assert(vrS() === "World")

    val vrI = id.newIntVar(1)
    assert(vrI() === 1)
    vrI() = vrI() + 2
    assert(vrI() === 3)

    val id2 = tx.newId()
    assert(id !== id2)

    def alwaysFindsTx()(implicit tx: S): Unit = implicitly[S]

    alwaysFindsTx()
  }
}
