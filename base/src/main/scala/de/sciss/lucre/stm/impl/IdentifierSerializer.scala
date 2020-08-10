///*
// *  IdentifierSerializer.scala
// *  (Lucre)
// *
// *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU Affero General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.lucre.stm
//package impl
//
//import de.sciss.serial.{DataInput, DataOutput, Serializer}
//
//final class IdentifierSerializer[S <: Base[S]] extends Serializer[S#Tx, S#Acc, S#Id] {
//  def write(id: S#Id, out: DataOutput): Unit = id.write(out)
//
//  def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): S#Id = tx.readId(in, access)
//}
