///*
// *  MappingNode.scala
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
//package de.sciss.lucre.event
//package impl
//
//import de.sciss.lucre.stm.Txn
//
///** A trait which combined external input events with self generated events. */
//trait MappingNode[T <: Txn[T], A, B]
//  extends Node[T] {
//
//  protected def inputEvent: EventLike[T, B]
//
//  /** Folds a new input event, by combining it with an optional previous output event. */
//  protected def foldUpdate(generated: Option[A], input: B)(implicit tx: T): Option[A]
//
//  trait Mapped extends GeneratorEvent[T, A] {
//    private[lucre] final def pullUpdate(pull: Pull[T])(implicit tx: T): Option[A] = {
//      val gen = if (pull.isOrigin(this)) Some(pull.resolve[A]) else None
//      if (pull.contains(inputEvent)) pull(inputEvent) match {
//        case Some(e)  => foldUpdate(gen, e)
//        case _        => gen
//      } else gen
//    }
//  }
//}