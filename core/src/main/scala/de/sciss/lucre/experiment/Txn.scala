package de.sciss.lucre.experiment

import java.io.Closeable

import scala.annotation.tailrec
import scala.concurrent.stm.Txn.ExternalDecider
import scala.concurrent.stm.{InTxn, InTxnEnd, TxnExecutor, TxnLocal, Txn => ScalaTxn}
import scala.util.control.NonFatal

object TxnLike {
  /** Implicitly extracts a Scala STM transaction from a `TxnLike` instance. */
  implicit def peer(implicit tx: TxnLike): InTxn = tx.peer

  /** Implicitly treats a Scala STM transaction as a `TxnLike` instance. */
  implicit def wrap(implicit peer: InTxn): TxnLike = new Wrapped(peer)

  private final class Wrapped(val peer: InTxn) extends TxnLike {
    override def toString: String = peer.toString

    def afterCommit (code: => Unit): Unit = ScalaTxn.afterCommit(_ => code)(peer)
  }
}
/** This is a minimal trait for any type of transactions that wrap an underlying Scala-STM transaction. */
trait TxnLike {
  /** Every transaction has a plain Scala-STM transaction as a peer. This comes handy for
   * setting up custom things like `TxnLocal`, `TMap`, or calling into the hooks of `concurrent.stm.Txn`.
   * It is also needed when re-wrapping the transaction of one system into another.
   */
  def peer: InTxn

  /** Registers a thunk to be executed after the transaction successfully committed. */
  def afterCommit(code: => Unit): Unit

  // will not be able to override this in Txn....
  // def beforeCommit(fun: TxnLike => Unit): Unit
}

object Txn {
  trait Resource extends Closeable with ExternalDecider

  private[this] final class Decider(var instances: scala.List[Resource])
    extends ExternalDecider {

    def shouldCommit(implicit txn: InTxnEnd): Boolean = instances match {
      case single :: Nil => single.shouldCommit
      case _ => commitAll(instances, Nil)
    }

    @tailrec
    private[this] def commitAll(remain: scala.List[Resource], done: scala.List[Resource])(implicit txn: InTxnEnd): Boolean =
      remain match {
        case head :: tail =>
          if (head.shouldCommit) {
            commitAll(tail, head :: done)
          } else {
            if (done.nonEmpty) {
              Console.err.println(s"Resource $head failed to commit transaction.")
              done.foreach { r =>
                Console.err.println(s"Closing $r as a precaution. The system must be re-opened prior to further use.")
                try {
                  r.close()
                } catch {
                  case NonFatal(e) => e.printStackTrace()
                }
              }
            }
            false
          }

        case _ => true
      }
  }

  private[this] val decider = TxnLocal[Decider]()

  private[lucre] def addResource(resource: Resource)(implicit tx: InTxn): Unit =
    if (decider.isInitialized) {
      decider().instances ::= resource
    } else {
      val d = new Decider(resource :: Nil)
      ScalaTxn.setExternalDecider(d)
      decider() = d
    }

  private[lucre] def requireEmpty(): Unit =
    if (!allowNesting && ScalaTxn.findCurrent.isDefined)
      throw new IllegalStateException("Nested transactions are not supported by this system.")

  private[this] var allowNesting = false

  def atomic[A](fun: InTxn => A): A = {
    requireEmpty()
    TxnExecutor.defaultAtomic(fun)
  }

  /** Allows to share a transaction between two systems, necessary
   * for a cross-system `Obj.copy` operation.
   */
  def copy[T1 <: Txn[T1], T2 <: Txn[T2], A](fun: (T1, T2) => A)
                                           (implicit cursor1: Cursor[T1], cursor2: Cursor[T2]): A = {
    cursor1.step { tx1 =>
      allowNesting = true // allow only for the next cursor step
      try {
        cursor2.step { implicit tx2 =>
          allowNesting = false
          fun(tx1, tx2)
        }
      } finally {
        allowNesting = false
      }
    }
  }
}

trait Txn[T <: Txn[T]] extends Exec[T] with TxnLike {
  //  def inMemory: S#I#Tx

  // ---- completion ----

  def beforeCommit(fun: T => Unit): Unit

  // ---- events ----

  private[experiment] def reactionMap: ReactionMap[T]

  // ---- attributes ----

  def attrMap(obj: Obj[T]): Obj.AttrMap[T]

  def attrMapOption(obj: Obj[T]): Option[Obj.AttrMap[T]]
}

trait AnyTxn extends Txn[AnyTxn]