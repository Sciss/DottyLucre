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

package de.sciss.lucre.stm

import de.sciss.lucre.stm

object Plain {
  implicit val instance: Plain = impl.PlainImpl()

  type Id = stm.Ident[Plain]
}
trait Plain extends Base /*[Plain]*/ with Cursor[Plain] with Exec[Plain] {
  type Tx     = Plain
  type Acc    = Unit
  
//  type Var[A] = stm.Var[Tx, A]
//  type Id     = Plain.Id

  type I      = Plain
}
