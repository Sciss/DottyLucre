package de.sciss

import scala.annotation.implicitNotFound

object ErasedTest {
  trait Sys {
    type Tx
  }
  
  trait Bla[S <: Sys](using erased final val s: S) {
    def apply()(implicit tx: s.Tx): Int
  }
}