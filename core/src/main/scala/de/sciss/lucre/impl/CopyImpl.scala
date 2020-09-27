/*
 *  CopyImpl.scala
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
package impl

import scala.collection.mutable
import scala.reflect.ClassTag

final class CopyImpl[In <: Txn[In], Out <: Txn[Out]](implicit txIn: In, txOut: Out)
  extends Copy[In, Out] {
  
  import scala.{None => Busy, Option => State, Some => Done}

  private[this] val stateMap = mutable.Map.empty[Elem[In], State[Elem[Out]]]
  private[this] val hintMap  = mutable.Map.empty[Elem[In], mutable.Map[String, Any]]
  private[this] var deferred: mutable.Buffer[() => Unit] = _

  private def newPhase(): Unit =
    deferred = mutable.Buffer.empty[() => Unit]

  newPhase()

  def defer[Repr[~ <: Txn[~]] <: Obj[~]](in: Repr[In], out: Repr[Out])(code: => Unit): Unit = {
    if (!stateMap.get(in).contains(Busy))
      throw new IllegalStateException(s"Copy.defer must be called during copy process: $in")
    stateMap.put(in, Done[Elem[Out]](out))
    deferred += (() => code)
  }

  def finish(): Unit =
    while (deferred.nonEmpty) {
      val d = deferred
      newPhase()
      d.foreach(_.apply())
    }

  private def copyImpl[Repr[~ <: Txn[~]] <: Elem[~]](in: Repr[In]): Repr[Out] = {
    stateMap.put(in, Busy)
    val out = in.copy()(txIn, txOut, this)
    stateMap.put(in, Done(out))
    out.asInstanceOf[Repr[Out]]
  }

  def apply[Repr[~ <: Txn[~]] <: Elem[~]](in: Repr[In]): Repr[Out] = {
    val opt: Option[State[Elem[Out]]] = stateMap.get(in)
    opt match {
      case Some(d: Done[Elem[Out]]) => d.value.asInstanceOf[Repr[Out]]
      case Some(Busy) => throw new IllegalStateException(s"Cyclic object graph involving $in")
      case None =>
        val out = copyImpl[Repr](in)
        // XXX TODO: currently Dotty crashes here: https://github.com/lampepfl/dotty/issues/9782
//        (in, out) match {
//          case (inObj: Obj[In], outObj: Obj[Out]) =>      // copy the attributes
//            // NOTE: do not use `defer` which should be reserved for the
//            // object itself only. Because if that object has called `defer`,
//            // it will be found already as `Done`, causing a second `defer`
//            // to throw an exception. Simply add to `deferred!
//            //     defer(inObj, outObj)(copyAttr(inObj, outObj)) // ...later
//            deferred += (() => copyAttr(inObj, outObj))
//          case _ =>
//        }

        if (in.isInstanceOf[Obj[_]] && out.isInstanceOf[Obj[_]]) {
          val inObj   = in.asInstanceOf[Obj[In  ]]
          val outObj  = in.asInstanceOf[Obj[Out ]]
          deferred += (() => copyAttr(inObj, outObj))
        }

        out
    }
  }

  def copyPlain[Repr[~ <: Txn[~]] <: Elem[~]](in: Repr[In]): Repr[Out] =
    stateMap.get(in) match {
      case Some(Done(out)) => out.asInstanceOf[Repr[Out]]
      case Some(Busy) => throw new IllegalStateException(s"Cyclic object graph involving $in")
      case None =>
        copyImpl(in)
    }

  def putHint[A](in: Elem[In], key: String, value: A): Unit = {
    val map = hintMap.getOrElse(in, {
      val res = mutable.Map.empty[String, Any]
      hintMap.put(in, res)
      res
    })
    map.put(key, value)
  }

  def getHint[A](in: Elem[In], key: String)(implicit ct: ClassTag[A]): Option[A] =
    hintMap.get(in).flatMap(_.get(key).collect {
      case a: A => a
    })

  def copyAttr(in: Obj[In], out: Obj[Out]): Unit =
    txIn.attrMapOption(in).foreach { inAttr =>
      val outAttr = out.attr
      inAttr.iterator.foreach { case (key, value) =>
        outAttr.put(key, apply(value))
      }
    }
}
