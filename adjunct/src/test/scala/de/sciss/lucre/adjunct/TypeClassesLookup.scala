package de.sciss.lucre.adjunct

import de.sciss.lucre.adjunct.Adjunct.{FromAny, HasDefault}

trait TypeClassesLookup {
  implicitly[FromAny[Int]]
  implicitly[FromAny[Boolean]]
  implicitly[FromAny[Double]]
  implicitly[FromAny[String]]
  implicitly[FromAny[Seq[Int]]]
  implicitly[FromAny[Seq[Double]]]
  implicitly[FromAny[Seq[Boolean]]]

  implicitly[HasDefault[Int]]
  implicitly[HasDefault[Boolean]]
  implicitly[HasDefault[Double]]
  implicitly[HasDefault[String]]
  implicitly[HasDefault[Seq[Int]]]
  implicitly[HasDefault[Seq[Double]]]
  implicitly[HasDefault[Seq[Boolean]]]
}