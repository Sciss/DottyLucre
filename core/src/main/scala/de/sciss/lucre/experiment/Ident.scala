/*
 *  Identifier.scala
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

package de.sciss.lucre.experiment

//import de.sciss.lucre.stm.impl.IdentifierSerializer
import de.sciss.serial
import de.sciss.serial.{DataInput, Serializer}

object Ident {
  //  implicit def serializer[S <: Base[S]]: Serializer[S#Tx, S#Acc, S#Id] =
  //    anySer.asInstanceOf[IdentifierSerializer[S]]
  //
  //  private val anySer = new IdentifierSerializer[NoBase]
}
trait Ident[T <: Exec[T]] 
  extends /*Disposable[Tx] with*/ serial.Writable {
  
  def dispose(): Unit

  def newVar[A](init: A)(implicit serializer: TxSerializer[T,/* Acc,*/ A]): Var[A]

  def newBooleanVar(init: Boolean): Var[Boolean]
  def newIntVar    (init: Int    ): Var[Int]
  def newLongVar   (init: Long   ): Var[Long]

  def readVar[A](in: DataInput)(implicit serializer: TxSerializer[T, /*Acc,*/ A]): Var[A]

  def readBooleanVar(in: DataInput): Var[Boolean]
  def readIntVar    (in: DataInput): Var[Int]
  def readLongVar   (in: DataInput): Var[Long]

  def ! (implicit tx: T): tx.Id = ???
}