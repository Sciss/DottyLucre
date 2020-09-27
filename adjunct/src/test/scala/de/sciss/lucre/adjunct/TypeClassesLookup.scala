/*
 *  TypeClassesLookup.scala
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

package de.sciss.lucre.adjunct

import de.sciss.lucre.Adjunct.{FromAny, HasDefault}

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