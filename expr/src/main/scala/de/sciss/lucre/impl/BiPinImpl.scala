/*
 *  BiPinImpl.scala
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

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.data.SkipList
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput, TFormat, WritableFormat}

import scala.collection.immutable.{IndexedSeq => Vec}

object BiPinImpl {
  import BiPin.{Added, Entry, Leaf, Modifiable, Moved, Removed, Update}

  type Tree[T <: Txn[T], A] = SkipList.Map[T, Long, Leaf[T, A]]

  def newEntry[T <: Txn[T], A <: Elem[T]](key: LongObj[T], value: A)(implicit tx: T): Entry[T, A] =
    if (Expr.isConst(key))     ConstEntry(              key, value)
    else                   new NodeEntry (Targets[T](), key, value).connect()

  def readEntry[T <: Txn[T], A <: Elem[T]](in: DataInput)(implicit tx: T): Entry[T, A] = {
    val tpe = in.readInt()
    if (tpe != Entry.typeId) sys.error(s"Type mismatch. Found $tpe, expected ${Entry.typeId}")
    in.readByte() match {
      case 3 =>
        val key     = LongObj.read(in)
        val value   = Elem.read(in).asInstanceOf[A]
        ConstEntry(key, value)

      case 0 =>
        val targets = Targets.readIdentified[T](in)
        readEntry(in, targets)
    }
  }

  def entryFormat[T <: Txn[T], A <: Elem[T]]: TFormat[T, Entry[T, A]] =
    anyEntryFmt.asInstanceOf[EntryFmt[T, A]]

  private val anyEntryFmt = new EntryFmt[AnyTxn, Obj[AnyTxn]]

  private final class EntryFmt[T <: Txn[T], A <: Elem[T]] extends WritableFormat[T, Entry[T, A]] {
    override def readT(in: DataInput)(implicit tx: T): Entry[T, A] = readEntry(in)
  }

  def readIdentifiedEntry[T <: Txn[T]](in: DataInput)(implicit tx: T): Elem[T] = {
    val targets = Targets.read[T](in)
    readEntry(in, targets)
  }

  private def readEntry[T <: Txn[T], A <: Elem[T]](in: DataInput, targets: Targets[T])
                                                  (implicit tx: T): Entry[T, A] = {
    val key     = LongObj.read(in)
    val value   = Elem.read(in).asInstanceOf[A]
    new NodeEntry(targets, key, value)
  }

  private trait EntryImpl[T <: Txn[T], A <: Elem[T]] extends Entry[T, A] {
    final def tpe: Elem.Type = Entry

    protected final def writeData(out: DataOutput): Unit = {
      key  .write(out)
      value.write(out)
    }
  }

  private case class ConstEntry[T <: Txn[T], A <: Elem[T]](key: LongObj[T], value: A)
    extends EntryImpl[T, A] with ConstElemImpl[T] {

    def changed: EventLike[T, Change[Long]] = DummyEvent[T, Change[Long]]

    private[lucre] override def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      ConstEntry(context(key), context[Elem](value))
  }

  private final class NodeEntry[T <: Txn[T], A <: Elem[T]](protected val targets: Targets[T],
                                                           val key: LongObj[T], val value: A)
    extends EntryImpl[T, A] with SingleEventNode[T, Change[Long]] {

    // ok, it's not really the "root", but the type is the same
    object changed extends Changed with RootEvent[T, Change[Long]]

    private[lucre] override def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new NodeEntry(Targets[Out](), context(key), context[Elem](value)).connect()

    protected def disposeData()(implicit tx: T): Unit = disconnect()

    def connect()(implicit tx: T): this.type = {
      key.changed ---> changed
      this
    }

    private[this] def disconnect()(implicit tx: T): Unit = {
      key.changed -/-> changed
    }
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] = {
    val targets = Targets.read(in)
    BiPinImpl.readImpl(in, targets)
  }

  private implicit def leafFormat[T <: Txn[T], A <: Elem[T]]: TFormat[T, Leaf[T, A]] =
    anyLeafFmt.asInstanceOf[LeafFmt[T, A]]

  private val anyLeafFmt = new LeafFmt[AnyTxn, Obj[AnyTxn]]

  private final class LeafFmt[T <: Txn[T], A <: Elem[T]] extends TFormat[T, Leaf[T, A]] {
    override def write(leaf: BiPin.Leaf[T, A], out: DataOutput): Unit = {
      val sz = leaf.size
      out.writeInt(sz)
      if (sz == 0) return
      leaf.foreach(_.write(out))
    }

    override def readT(in: DataInput)(implicit tx: T): BiPin.Leaf[T, A] = {
      val sz = in.readInt()
      if (sz == 0) Vector.empty
      else Vector.fill(sz)(readEntry(in))
    }
  }

  def newModifiable[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]](implicit tx: T): Modifiable[T, E[T]] =
    new Impl1[T, E](Targets[T]()) {
      val tree: Tree[T, A] = newTree()
    }

  def format[T <: Txn[T], A <: Elem[T]]: TFormat[T, BiPin[T, A]] =
    anyFmt.asInstanceOf[Fmt[T, A, BiPin[T, A]]]

  def modifiableFormat[T <: Txn[T], A <: Elem[T]]: TFormat[T, BiPin.Modifiable[T, A]] =
    anyFmt.asInstanceOf[Fmt[T, A, BiPin.Modifiable[T, A]]]

  private val anyFmt = new Fmt[AnyTxn, Obj[AnyTxn], BiPin[AnyTxn, Obj[AnyTxn]]]

  private def readImpl[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]](in: DataInput, targets: Targets[T])
                                                              (implicit tx: T): Impl1[T, E] =
    new Impl1[T, E](targets) {
      val tree: Tree[T, A] = readTree(in)
    }

  private class Fmt[T <: Txn[T], A <: Elem[T], Repr <: BiPin[T, A]]
    extends ObjFormat[T, Repr] {

    def tpe: Obj.Type = BiPin
  }

  final def copyTree[In <: Txn[In], Out <: Txn[Out], E[~ <: Txn[~]] <: Elem[~],
    Repr <: Impl[Out, E, Repr]](in: Tree[In, E[In]], out: Tree[Out, E[Out]], outImpl: Repr)
                               (implicit txIn: In, txOut: Out, context: Copy[In, Out]): Unit = {
    type EntryAux[~ <: Txn[~]] = Entry[~, E[~]]
    in.iterator.foreach { case (time, xsIn) =>
      val xsOut = xsIn.map(e => context[EntryAux](e))
      out.put(time, xsOut)
      xsOut.foreach { entry =>
        outImpl.changed += entry
      }
    }
  }

  abstract class Impl[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~], Repr <: Modifiable[T, E[T]]]
    extends Modifiable[T, E[T]]
      with SingleEventNode[T, Update[T, E[T], Repr]] {

    pin: Repr =>

    type A = E[T]

    // ---- abstract ----
    protected def tree: Tree[T, E[T]]

    // ---- impl ----

    final def isEmpty (implicit tx: T): Boolean = tree.isEmpty
    final def nonEmpty(implicit tx: T): Boolean = !isEmpty

    protected type PinAux[~ <: Txn[~]] = BiPin[~, E[~]]

    protected final def newTree()(implicit tx: T): Tree[T, A] =
      SkipList.Map.empty[T, Long, Leaf[T, A]]()

    protected final def readTree(in: DataInput)(implicit tx: T): Tree[T, A] =
      SkipList.Map.read[T, Long, Leaf[T, E[T]]](in)

    // ---- event behaviour ----

    object changed extends Changed with GeneratorEvent[T, Update[T, A, Repr]] with Caching {
      def += (elem: Entry[T, A])(implicit tx: T): Unit = elem.changed ---> this
      def -= (elem: Entry[T, A])(implicit tx: T): Unit = elem.changed -/-> this

      def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Update[T, A, Repr]] = {
        if (pull.isOrigin(this)) return Some(pull.resolve)

        val changes: List[Moved[T, A]] = pull.parents(this).iterator.flatMap { evt =>
          val entry = evt.node
          val opt   = pull(evt).map { ch =>
            Moved(ch.asInstanceOf[Change[Long]], entry.asInstanceOf[Entry[T, A]])
          }
          opt
        } .toList

        if (changes.isEmpty) None
        else {
          changes.foreach {
            case Moved(timeChange, entry) =>
              if (timeChange.isSignificant) {
                remove1   (timeChange.before, entry.key, entry.value, fireAndDispose = false)
                addNoFire (timeChange.now   , entry)
              }
          }
          Some(Update[T, A, Repr](pin, changes))
        }
      }
    }

    protected final def disposeData()(implicit tx: T): Unit = {
      tree.iterator.foreach { case (_, xs) =>
        xs.foreach { entry =>
          entry.dispose()
        }
      }
      tree.dispose()
    }

    protected final def writeData(out: DataOutput): Unit = tree.write(out)

    // ---- collection behaviour ----

    final def clear()(implicit tx: T): Unit = {
      val it = tree.iterator
      if (it.hasNext) {
        val changes = it.toList.flatMap {
          case (timeVal, seq) =>
            seq.map { elem =>
              Removed[T, A](timeVal, elem)
            }
        }
        tree.clear()
        changed.fire(Update(this, changes))
      }
    }

    final def add(key: LongObj[T], value: A)(implicit tx: T): Unit = {
      val timeVal = key.value // timeValue
      val entry   = newEntry(key, value)
      addNoFire(timeVal, entry)

      changed += entry
      changed.fire(Update(pin, Added(timeVal, entry) :: Nil))
    }

    final def intersect(time: Long)(implicit tx: T): Leaf[T, A] = tree.floor(time) match {
      case Some((_, seq)) => seq
      case _ => Vec.empty
    }

    final def eventAfter (time: Long)(implicit tx: T): Option[Long] = tree.ceil (time + 1).map(_._1)
    final def eventBefore(time: Long)(implicit tx: T): Option[Long] = tree.floor(time - 1).map(_._1)

    final def at     (time: Long)(implicit tx: T): Option[Entry[T, A]] = intersect(time).headOption
    final def valueAt(time: Long)(implicit tx: T): Option[         A ] = intersect(time).headOption.map(_.value)
    final def floor  (time: Long)(implicit tx: T): Option[Entry[T, A]] = tree.floor(time).flatMap(_._2.headOption)
    final def ceil   (time: Long)(implicit tx: T): Option[Entry[T, A]] = tree.ceil (time).flatMap(_._2.headOption)

    /*
     * Adds a new value, and returns the dirty which corresponds to the new region holding `elem`.
     *
     * @param timeVal the time value at which the new element is inserted
     * @param elem    the element which is inserted
     * @return
     */
    private[this] def addNoFire(timeVal: Long, elem: Entry[T, A])(implicit tx: T): Unit =
      tree.get(timeVal) match {
        case Some(oldLeaf) =>
          tree += timeVal -> (elem +: oldLeaf)
        case _ =>
          tree += timeVal -> Vector(elem)
      }

    final def remove(key: LongObj[T], value: A)(implicit tx: T): Boolean = {
      val timeVal = key.value // timeValue
      val found = remove1(timeVal, key, value, fireAndDispose = true)
      found
    }

    private[this] final def entryRemoved(timeVal: Long, entry: Entry[T, A], visible: Boolean)
                                        (implicit tx: T): Unit = {
      changed -= entry
      if (visible) changed.fire(Update(pin, Removed(timeVal, entry) :: Nil))
      entry.dispose()
    }

    private[this] final def remove1(timeVal: Long, key: LongObj[T], value: A, fireAndDispose: Boolean)
                                   (implicit tx: T): Boolean = {
      tree.get(timeVal) match {
        case Some(Vec(single)) =>
          val found = single.key == key && single.value == value
          if (found) {
            tree -= timeVal
            if (fireAndDispose) entryRemoved(timeVal, single, visible = true)
          }
          found

        case Some(seq) =>
          val i       = seq.indexWhere(entry => entry.key == key && entry.value == value) // indexOf(elem)
          val found   = i >= 0
          if (found) {
            val visible = i == 0
            val entry   = seq(i)
            val seqNew  = seq.patch(i, Vec.empty[Entry[T, A]], 1)
            tree += timeVal -> seqNew
            if (fireAndDispose) entryRemoved(timeVal, entry, visible = visible)
          }
          found

        case None => false
      }
    }

    def debugList(implicit tx: T): List[(Long, A)] =
      tree.toList.flatMap {
        case (time, seq) => seq.map(time -> _.value)
      }
  }

  private abstract class Impl1[T <: Txn[T], E[~ <: Txn[~]] <: Elem[~]](protected val targets: Targets[T])
    extends Impl[T, E, Impl1[T, E]] { in =>

    def tpe: Obj.Type = BiPin

    override def toString: String = s"BiPin${tree.id}"

    final def modifiableOption: Option[BiPin.Modifiable[T, A]] = Some(this)

    private[lucre] final def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out, context: Copy[T, Out]): Elem[Out] = {
      new Impl1[Out, E](Targets[Out]()) { out =>
        val tree: Tree[Out, A] = out.newTree()
        context.defer[PinAux](in, out)(copyTree[T, Out, E, Impl1[Out, E]](in.tree, out.tree, out))
        // out.connect()
      }
    }
  }
}