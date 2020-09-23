/*
 *  Workspace.scala
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

object Workspace {
  object Implicits {
    implicit def dummy[T <: Txn[T]](implicit system: Sys, cursor: Cursor[T]): Workspace[T] =
      new DummyImpl[T](system, cursor)
    // dummyVal.asInstanceOf[DummyImpl[T]]

    //    private val dummyVal = new DummyImpl[NoSys]

    private final case class DummyImpl[T <: Txn[T]](system: Sys, cursor: Cursor[T])
      extends Workspace[T] {

      def addDependent(dep: Disposable[T])(implicit tx: TxnLike): Unit = ()
      def removeDependent(dep: Disposable[T])(implicit tx: TxnLike): Unit = ()

      def dependents(implicit tx: TxnLike): Iterable[Disposable[T]] = Nil

      def folder: Option[File] = None

      def name: String = "dummy"

      def close(): Unit = ()

      def dispose()(implicit tx: T): Unit = ()

      def root(implicit tx: T): Folder[T] =
        throw new UnsupportedOperationException("No root folder on a dummy workspace handle")
    }
  }
}
trait Workspace[T <: Txn[T]] extends Disposable[T] {
  implicit def system: Sys

  def cursor: Cursor[T]

  def folder: Option[File]

  def name: String

  /** Issues a transaction that closes and disposes the workspace. */
  def close(): Unit

  /** Adds a dependent which is disposed just before the workspace is disposed.
   *
   * @param dep  the dependent. This must be an _ephemeral_ object.
   */
  def addDependent(dep: Disposable[T])(implicit tx: TxnLike /* T */): Unit
  def removeDependent(dep: Disposable[T])(implicit tx: TxnLike /* T */): Unit

  def dependents(implicit tx: TxnLike): Iterable[Disposable[T]]

  def root(implicit tx: T): Folder[T]
}
