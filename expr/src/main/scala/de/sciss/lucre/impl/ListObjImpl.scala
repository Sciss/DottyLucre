/*
 *  ListObjImpl.scala
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
package impl

import de.sciss.lucre.Event.Targets
import de.sciss.serial.{DataInput, DataOutput, TFormat}

import scala.annotation.{switch, tailrec}

object ListObjImpl {
  import ListObj.Modifiable

  def newModifiable[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]](implicit tx: T): Modifiable[T, E[T]] =
    new Impl1[T, E] {
      protected val targets: Targets[T]   = Targets[T]()
      protected val sizeRef: Var[T, Int]  = id.newIntVar(0)
      protected val headRef: Var[T, C]    = id.newVar[C](null)(tx, CellFmt)
      protected val lastRef: Var[T, C]    = id.newVar[C](null)(tx, CellFmt)
    }

  def format[T <: Txn[T], A <: Elem[T]]: TFormat[T, ListObj[T, A]] =
    anyFmt.asInstanceOf[Fmt[T, A]]

  private val anyFmt = new Fmt[AnyTxn, Obj[AnyTxn]]

  def modFormat[T <: Txn[T], A <: Elem[T]]: TFormat[T, Modifiable[T, A]] =
    anyModFmt.asInstanceOf[ModFmt[T, A]]

  private val anyModFmt = new ModFmt[AnyTxn, Obj[AnyTxn]]

  private class Fmt[T <: Txn[T], A <: Elem[T]] extends ObjFormat[T, ListObj[T, A]] {
    def tpe: Obj.Type = ListObj
  }

  private class ModFmt[T <: Txn[T], A <: Elem[T]] extends ObjFormat[T, Modifiable[T, A]] {
    def tpe: Obj.Type = ListObj
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] = {
    val targets = Targets.read[T](in)
    ListObjImpl.read(in, targets)
  }

  private def read[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]](in: DataInput, _targets: Targets[T])
                                                          (implicit tx: T): Impl1[T, E] =
    new Impl1[T, E] {
      protected val targets: Targets[T]   = _targets
      protected val sizeRef: Var[T, Int]  = id.readIntVar(in)
      protected val headRef: Var[T, C]    = id.readVar[C](in)
      protected val lastRef: Var[T, C]    = id.readVar[C](in)
    }

  final class Cell[T <: Txn[T], A](val elem: A,
                                   val pred: Var[T, Cell[T, A]],
                                   val succ: Var[T, Cell[T, A]])

  private final class Iter[T <: Txn[T], A](private var cell: Cell[T, A])(implicit tx: T) extends Iterator[A] {
    override def toString: String = if (cell == null) "empty iterator" else "non-empty iterator"

    def hasNext: Boolean = cell != null

    def next(): A = {
      if (cell == null) throw new NoSuchElementException("next on empty iterator")
      val res = cell.elem
      cell    = cell.succ()
      res
    }
  }

  private def copyList[In <: Txn[In], Out <: Txn[Out], E[~ <: Txn[~]] <: Elem[~]](in : Modifiable[In , E[In ]],
                                                                                  out: Modifiable[Out, E[Out]])
                                                                                 (implicit txIn: In, txOut: Out,
                                                                                  context: Copy[In, Out]): Unit = {
    in.iterator.foreach { elem =>
      out.addLast(context(elem))
    }
  }

  abstract class Impl[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~], Repr <: Modifiable[T, E[T]]]
    extends Modifiable[T, E[T]] with SingleEventNode[T, ListObj.Update[T, E[T], Repr]] {

    list: Repr =>

    type A = E[T]
    protected type ListAux[~ <: Txn[~]] = ListObj[~, E[~]]

    final protected type C = Cell[T, A]

    protected def headRef: Var[T, C]
    protected def lastRef: Var[T, C]
    protected def sizeRef: Var[T, Int]

    // ---- event behaviour ----

    protected implicit object CellFmt extends TFormat[T, C] { self =>
      override def write(cell: C, out: DataOutput): Unit =
        if (cell != null) {
          out.writeByte(1)
          cell.elem.write(out)
          cell.pred.write(out)
          cell.succ.write(out)
        } else {
          out.writeByte(0)
        }

      override def readT(in: DataInput)(implicit tx: T): C =
        (in.readByte: @switch) match {
          case 1 =>
            val elem = Elem.read(in).asInstanceOf[A]
            val pred = id.readVar[C](in)(self)
            val succ = id.readVar[C](in)(self)
            new Cell[T, A](elem, pred, succ)
          case 0 => null
          case cookie => sys.error(s"Unexpected cookie $cookie")
        }
    }

    // protected def reader: evt.Reader[T, List[T, A, U]]

    object changed extends Changed
      with RootGeneratorEvent[T, ListObj.Update[T, A, Repr]]

    final def indexOf(elem: A)(implicit tx: T): Int = {
      var idx = 0
      var rec = headRef()
      while (rec != null) {
        if (rec.elem == elem) return idx
        idx += 1
        rec = rec.succ()
      }
      -1
    }

    final def apply(idx: Int)(implicit tx: T): A =
      get(idx).getOrElse(throw new IndexOutOfBoundsException(idx.toString))

    final def get(idx: Int)(implicit tx: T): Option[A] = {
      if (idx < 0) return None
      var left = idx
      var rec = headRef()
      while (rec != null && left > 0) {
        left -= 1
        rec = rec.succ()
      }
      if (rec == null) None else Some(rec.elem)
    }

    final def addLast(elem: A)(implicit tx: T): Unit = {
      val pred      = lastRef()
      val succ      = null
      val idx       = sizeRef()
      insert(elem, pred, succ, idx)
    }

    final def addHead(elem: A)(implicit tx: T): Unit = {
      val pred      = null
      val succ      = headRef()
      val idx       = 0
      insert(elem, pred, succ, idx)
    }

    def insert(index: Int, elem: A)(implicit tx: T): Unit = {
      if (index < 0)      throw new IndexOutOfBoundsException(index.toString)
      var pred      = null: C
      var succ      = headRef()
      var idx       = 0
      while (idx < index) {
        if (succ == null) throw new IndexOutOfBoundsException(index.toString)
        pred  = succ
        succ  = succ.succ()
        idx  += 1
      }
      insert(elem, pred, succ, idx)
    }

    private[this] def insert(elem: A, pred: C, succ: C, idx: Int)(implicit tx: T): Unit = {
      val recPred   = id.newVar[C](pred)
      val recSucc   = id.newVar[C](succ)
      val rec       = new Cell[T, A](elem, recPred, recSucc)
      val predSucc  = if (pred == null) headRef else pred.succ
      val succPred  = if (succ == null) lastRef else succ.pred
      predSucc()    = rec
      succPred()    = rec
      sizeRef()     = sizeRef() + 1

      fireAdded(idx, elem)
    }

    final protected def foreach(fun: A => Unit)(implicit tx: T): Unit = {
      @tailrec def loop(cell: C): Unit =
        if (cell != null) {
          fun(cell.elem)
          loop(cell.succ())
        }

      loop(headRef())
    }

    private[this] def fireAdded(idx: Int, elem: A)(implicit tx: T): Unit =
      changed.fire(ListObj.Update(list, Vector(ListObj.Added(idx, elem))))

    private[this] def fireRemoved(idx: Int, elem: A)(implicit tx: T): Unit =
      changed.fire(ListObj.Update(list, Vector(ListObj.Removed(idx, elem))))

    final def remove(elem: A)(implicit tx: T): Boolean = {
      var rec = headRef()
      var idx = 0
      while (rec != null) {
        if (rec.elem == elem) {
          removeCell(rec)
          fireRemoved(idx, elem)
          return true
        }
        rec = rec.succ()
        idx += 1
      }
      false
    }

    final def removeAt(index: Int)(implicit tx: T): A = {
      if (index < 0) throw new IndexOutOfBoundsException(index.toString)
      var rec = headRef()
      if (rec == null) throw new IndexOutOfBoundsException(index.toString)
      var idx = 0
      while (idx < index) {
        rec = rec.succ()
        if (rec == null) throw new IndexOutOfBoundsException(index.toString)
        idx += 1
      }

      val e = rec.elem
      removeCell(rec)
      fireRemoved(idx, e)
      e
    }

    // unlinks a cell and disposes it. does not fire. decrements sizeRef
    private[this] def removeCell(cell: C)(implicit tx: T): Unit = {
      val pred = cell.pred()
      val succ = cell.succ()
      if (pred != null) {
        pred.succ() = succ
      } else {
        headRef() = succ
      }
      if (succ != null) {
        succ.pred() = pred
      } else {
        lastRef() = pred
      }
      sizeRef() = sizeRef() - 1 // .transform(_ - 1)
      disposeCell(cell)
    }

    final def removeLast()(implicit tx: T): A = {
      val rec = lastRef()
      if (rec == null) throw new NoSuchElementException("last of empty list")

      val pred  = rec.pred()
      val e     = rec.elem
      val idx   = sizeRef() - 1
      disposeCell(rec)
      sizeRef() = idx
      lastRef() = pred
      if (pred == null) {
        headRef() = null
      } else {
        pred.succ() = null
      }
      fireRemoved(idx, e)
      e
    }

    final def removeHead()(implicit tx: T): A = {
      val rec = headRef()
      if (rec == null) throw new NoSuchElementException("head of empty list")

      val succ = rec.succ()
      val e = rec.elem
      disposeCell(rec)
      sizeRef() = sizeRef() - 1 // .transform(_ - 1)
      headRef() = succ
      if (succ == null) {
        lastRef() = null
      } else {
        succ.pred() = null
      }
      fireRemoved(0, e)
      e
    }

    final def clear()(implicit tx: T): Unit =
      while (nonEmpty) removeLast()

    // deregisters element event. disposes cell contents, but does not unlink, nor fire.
    private[this] def disposeCell(cell: C)(implicit tx: T): Unit = {
      // unregisterAent(cell.elem)
      cell.pred.dispose()
      cell.succ.dispose()
    }

    final protected def disposeData()(implicit tx: T): Unit = {
      var rec = headRef()
      while (rec != null) {
        val tmp = rec.succ()
        disposeCell(rec)
        rec = tmp
      }
      sizeRef.dispose()
      headRef.dispose()
      lastRef.dispose()
    }

    final protected def writeData(out: DataOutput): Unit = {
      sizeRef.write(out)
      headRef.write(out)
      lastRef.write(out)
    }

    final def isEmpty (implicit tx: T): Boolean = size == 0
    final def nonEmpty(implicit tx: T): Boolean = size > 0
    final def size    (implicit tx: T): Int     = sizeRef()

    final def headOption(implicit tx: T): Option[A] = {
      val rec = headRef()
      if (rec != null) Some(rec.elem) else None
    }

    final def lastOption(implicit tx: T): Option[A] = {
      val rec = lastRef()
      if (rec != null) Some(rec.elem) else None
    }

    final def head(implicit tx: T): A = {
      val rec = headRef()
      if (rec != null) rec.elem else throw new NoSuchElementException("head of empty list")
    }

    final def last(implicit tx: T): A = {
      val rec = lastRef()
      if (rec != null) rec.elem else throw new NoSuchElementException("last of empty list")
    }

    final def iterator(implicit tx: T): Iterator[A] = new Iter[T, A](headRef())
  }

  private abstract class Impl1[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]] extends Impl[T, E, Impl1[T, E]] {
    in =>

    final def tpe: Obj.Type = ListObj

    override def toString = s"ListObj$id"

    def modifiableOption: Option[Modifiable[T, A]] = Some(this)

    final override private[lucre] def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out,
                                                              context: Copy[T, Out]): Elem[Out] = {
      val out = newModifiable[Out, E]
      context.defer[ListAux](in, out)(copyList[T, Out, E](in, out)(txIn, txOut, context))
      // .connect
      out
    }
  }
}