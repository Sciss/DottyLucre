/*
 *  ArtifactLocation.scala
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

import java.io.File

import de.sciss.lucre
import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.ExprTypeImpl
import de.sciss.serial.{ConstFormat, TFormat}

object ArtifactLocation extends ExprTypeImpl[File, ArtifactLocation] {
  import lucre.{ArtifactLocation => Repr}

  final val typeId = 0x10003

  def tmp[T <: Txn[T]]()(implicit tx: T): Const[T] = {
    val dir   = File.createTempFile("artifacts", "tmp")
    dir.delete()
    dir.mkdir()
    dir.deleteOnExit()
    newConst(dir)
  }

  implicit override def valueFormat: ConstFormat[File] = TFormat.File

  def tryParse(value: Any): Option[File] = value match {
    case loc: File  => Some(loc)
    case _          => None
  }

  protected override def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  protected override def mkVar[T <: Txn[T]](targets: Targets[T], vr: lucre.Var[E[T]], connect: Boolean)
                                  (implicit tx: T): Var[T] = {
    val res = new _Var[T](tx, targets, vr)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn[T]](val tx: T, val targets: Targets[T], val ref: lucre.Var[E[T]])
    extends VarImpl[T] with Repr[T]
}
/** An artifact location is a directory on an external storage. */
trait ArtifactLocation[T <: Txn[T]] extends Expr[T, File] {
  /** Alias for `value` */
  def directory: File = value
}