package de.sciss.lucre.experiment

import scala.concurrent.stm.{InTxn, InTxnEnd, TxnExecutor, TxnLocal, Txn => ScalaTxn}

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


trait Txn[T <: Txn[T]] extends Exec[T] with TxnLike {
  //  def inMemory: S#I#Tx

  // ---- completion ----

  def beforeCommit(fun: T => Unit): Unit

  // ---- events ----

//  private[lucre] def reactionMap: ReactionMap[T]

  // ---- attributes ----

  def attrMap(obj: Obj[T]): Obj.AttrMap[T]

  def attrMapOption(obj: Obj[T]): Option[Obj.AttrMap[T]]
}

trait AnyTxn extends Txn[AnyTxn]