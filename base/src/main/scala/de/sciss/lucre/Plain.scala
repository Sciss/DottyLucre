/*
 *  Plain.scala
 *  (Lucre)
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

import de.sciss.lucre.{Base, Exec, Ident, Var}
import de.sciss.lucre.impl.PlainImpl
import de.sciss.lucre

object Plain {
  implicit val instance: Plain = PlainImpl()

  type Id = Ident[Plain]
}
trait Plain extends Base /*[Plain]*/ with Cursor[Plain] with Exec[Plain] {
  type Tx     = Plain
  type Acc    = Unit

  type Var[A] = lucre.Var[A]
  type Id     = Plain.Id

  type I      = Plain
}
