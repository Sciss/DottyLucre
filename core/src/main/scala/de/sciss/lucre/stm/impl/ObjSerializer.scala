///*
// *  ObjSerializer.scala
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
//package de.sciss.lucre.stm.impl
//
//import de.sciss.equal.Implicits._
//import de.sciss.lucre.stm.{Obj, TxSerializer, Txn}
//import de.sciss.serial
//import de.sciss.serial.{DataInput, DataOutput}
//
//trait ObjSerializer[T <: Txn[T], Repr <: Obj[T]]
//  extends TxSerializer[T, Repr] {
//
//  protected def tpe: Obj.Type
//
//  final def write(v: Repr, out: DataOutput): Unit = v.write(out)
//
//  final def read(in: DataInput, tx: T)(implicit acc: tx.Acc): Repr = {
//    val tpe0 = in.readInt()
//    if (tpe0 !== tpe.typeId) sys.error(s"Type mismatch, expected ${tpe.typeId}, found $tpe0")
//    tpe.readIdentifiedObj(in, tx).asInstanceOf[Repr]
//  }
//}