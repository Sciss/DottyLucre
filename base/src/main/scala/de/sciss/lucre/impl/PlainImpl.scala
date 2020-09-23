/*
 *  PlainImpl.scala
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

import de.sciss.lucre.Plain.Id
import de.sciss.serial.{DataInput, DataOutput, TFormat, Writable}

object PlainImpl {
  def apply(): Plain = new SysImpl

  private def opNotSupported(name: String): Nothing = sys.error(s"Operation not supported: $name")

  type T = Plain

  private final class IdImpl extends Ident[Plain] {
    override def toString = s"Plain.Id@${hashCode.toHexString}"
    
    def dispose()(implicit tx: T): Unit = ()

    def !(implicit tx: Plain): Id = this

    def write(out: DataOutput): Unit = opNotSupported("Plain.Id.write")
    
    def newVar[A](init: A)(implicit tx: T, format: TFormat[Plain, A]): Var[T, A] = new VarImpl(init)

    def newBooleanVar (init: Boolean)(implicit tx: T): Var[T, Boolean] = new BooleanVarImpl (init)
    def newIntVar     (init: Int    )(implicit tx: T): Var[T, Int    ] = new IntVarImpl     (init)
    def newLongVar    (init: Long   )(implicit tx: T): Var[T, Long   ] = new LongVarImpl    (init)

    def readVar[A](in: DataInput)(implicit tx: T, format: TFormat[Plain, A]): Var[T, A] =
      opNotSupported("readVar")

    def readBooleanVar(in: DataInput)(implicit tx: T): Var[T, Boolean] = opNotSupported("readBooleanVar" )
    def readIntVar    (in: DataInput)(implicit tx: T): Var[T, Int    ] = opNotSupported("readIntVar"     )
    def readLongVar   (in: DataInput)(implicit tx: T): Var[T, Long   ] = opNotSupported("readLongVar"    )
  }

  private abstract class AbstractVar extends Disposable[T] with Writable {
    final def dispose()(implicit tx: T): Unit = ()

    final def write(out: DataOutput): Unit = opNotSupported("Plain.Var.write")
  }

  private final class VarImpl[A](private[this] var value: A)
    extends AbstractVar with Var[T, A] {

    def apply()(implicit tx: T): A = value

    def update(v: A)(implicit tx: T): Unit = value = v

    def swap(v: A)(implicit tx: T): A = {
      val res = value
      value = v
      res
    }
  }

  private final class BooleanVarImpl(private[this] var value: Boolean)
    extends AbstractVar with Var[T, Boolean] {

    def apply()(implicit tx: T): Boolean = value

    def update(v: Boolean)(implicit tx: T): Unit = value = v

    def swap(v: Boolean)(implicit tx: T): Boolean = {
      val res = value
      value = v
      res
    }
  }

  private final class IntVarImpl(private[this] var value: Int)
    extends AbstractVar with Var[T, Int] {

    def apply()(implicit tx: T): Int = value

    def update(v: Int)(implicit tx: T): Unit = value = v

    def swap(v: Int)(implicit tx: T): Int = {
      val res = value
      value = v
      res
    }
  }

  private final class LongVarImpl(private[this] var value: Long)
    extends AbstractVar with Var[T, Long] {

    def apply()(implicit tx: T): Long = value

    def update(v: Long)(implicit tx: T): Unit = value = v

    def swap(v: Long)(implicit tx: T): Long = {
      val res = value
      value = v
      res
    }
  }

  private final class SysImpl extends Plain {
    type S = Plain

    val s: S = this

    override def toString = "Plain"

    // ---- Base ----

    def close(): Unit = ()

    def inMemory: I = this

    def inMemoryTx(tx: Tx): Tx = tx

    // ---- Cursor ----

    def step[A](fun: Tx => A): A = fun(this)

    def stepTag[A](systemTimeNanos: Long)(fun: Tx => A): A = fun(this)

    // ---- Executor ----

    val system: S = this

    //    type Id = Ident[Plain]

    def newId(): Id = new IdImpl

    def readId(in: DataInput): Id = opNotSupported("readId")

//    def newRef[A](init: A): Ref[Tx, A] = new VarImpl(init)

//    def newVar[A](id: Id, init: A)(implicit format: TxFormat[Tx, A]): Var[A] =
//      new VarImpl(init)
//
//    def newBooleanVar (id: Id, init: Boolean ): Var[Boolean] = new BooleanVarImpl (init)
//    def newIntVar     (id: Id, init: Int     ): Var[Int]     = new IntVarImpl     (init)
//    def newLongVar    (id: Id, init: Long    ): Var[Long]    = new LongVarImpl    (init)
//
//    def newVarArray[A](size: Int): Array[Var[A]] = new Array[Var[A]](size)

    def newIdentMap[A]: IdentMap[Id, Tx, A] = new PlainIdentMap[A]

    //    def newInMemoryMap[K, V]: RefMap[S, K, V] = new PlainInMemoryMap[K, V]
    //    def newInMemorySet[A]   : RefSet[S, A]    = new PlainInMemorySet[A]

//    def readVar[A](id: Id, in: DataInput)(implicit format: TFormat[Tx, A]): Var[A] =
//      opNotSupported("readVar")
//
//    def readBooleanVar(id: Id, in: DataInput): Var[Boolean]  = opNotSupported("readBooleanVar")
//    def readIntVar    (id: Id, in: DataInput): Var[Int]      = opNotSupported("readIntVar")
//    def readLongVar   (id: Id, in: DataInput): Var[Long]     = opNotSupported("readLongVar")

    def newHandle[A](value: A)(implicit format: TFormat[Tx, A]): Source[Tx, A] =
      new EphemeralSource(value)
  }
}