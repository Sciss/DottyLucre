/*
 *  DataStore.scala
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

import java.io.Closeable

import de.sciss.serial.{DataInput, DataOutput}

object DataStore {
  trait Factory {
    /** Opens a new database within the given storage environment.
     *
     * @param name       the name of the database
     * @param overwrite  whether to overwrite (`true`) an existing database by that name if it exists,
     *                   or not (`false`, default)
     * @return           the newly opened database
     */
    def open(name: String, overwrite: Boolean = false): DataStore
  }
}
trait DataStore extends Closeable {
  def put     (keyFun: DataOutput => Unit)(valueFun: DataOutput => Unit)(implicit tx: TxnLike): Unit
  def get[A]  (keyFun: DataOutput => Unit)(valueFun: DataInput => A)(    implicit tx: TxnLike): Option[A]
  def contains(keyFun: DataOutput => Unit)(implicit tx: TxnLike): Boolean
  def remove  (keyFun: DataOutput => Unit)(implicit tx: TxnLike): Boolean

  def flatGet[A](keyFun: DataOutput => Unit)(valueFun: DataInput => Option[A])(implicit tx: TxnLike) : Option[A]

  def numEntries(implicit tx: TxnLike): Int
}
