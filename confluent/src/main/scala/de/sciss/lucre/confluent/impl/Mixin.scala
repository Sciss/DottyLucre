/*
 *  Mixin.scala
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

package de.sciss.lucre.confluent
package impl

import de.sciss.lucre.confluent.Log.log
import de.sciss.lucre.confluent.impl.DurableCacheMapImpl.Store
import de.sciss.lucre.confluent.impl.{PathImpl => Path}
import de.sciss.lucre.data.Ancestor
import de.sciss.lucre.impl.{ReactionMapImpl, TRandomImpl}
import de.sciss.lucre.{ConfluentLike, DataStore, Ident, IdentMap, ConstantSerializer, Observer, TRandom, TSerializer, TSource, TxnLike, Txn => LTxn}
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{InTxn, TxnExecutor}

trait Mixin[Tx <: Txn[Tx]]
  extends ConfluentLike[Tx] with IndexMapHandler[Tx] with PartialMapHandler[Tx] with ReactionMapImpl.Mixin[Tx] {

  self =>

  // ---- abstract methods ----

  protected def storeFactory: DataStore.Factory

  protected def wrapRegular(dtx: D, inputAccess: Access[T], retroactive: Boolean, cursorCache: Cache[T],
                            systemTimeNanos: Long): T

  protected def wrapRoot(peer: InTxn): T

  def durableTx(tx: T): D

  // ---- init ----

  final val store: DataStore = storeFactory.open("k-main")

  private[this] val varMap = DurablePersistentMap.newConfluentIntMap[T](store, this, isOblivious = false)

  final val fullCache: CacheMap.Durable[T, Int, Store[T, Int]] = DurableCacheMapImpl.newIntCache[T](varMap)

  final protected val eventMap: IdentMap[Ident[T], T, Map[Int, List[Observer[T, _]]]] = {
    val map = InMemoryConfluentMap.newIntMap[T]
    new InMemoryIdMapImpl[T, Map[Int, List[Observer[T, _]]]](map)
  }

  private val global: GlobalState[T, D] =
    durable.step { implicit tx =>
      val root = durable.rootJoin { implicit tx =>
        val durRootId     = durable.newIdValue() // stm.DurableSurgery.newIdValue(durable)
        val idCnt         = tx.newCachedIntVar(0)
        val versionLinear = tx.newCachedIntVar(0)
        val versionRandom = tx.newCachedLongVar(TRandomImpl.initialScramble(0L)) // scramble !!!
        val partialTree   = Ancestor.newTree[D, Long](1L << 32)(tx, implicitly[TSerializer[D, Long]], _.toInt)
        GlobalState[T, D](durRootId = durRootId, idCnt = idCnt, versionLinear = versionLinear,
          versionRandom = versionRandom, partialTree = partialTree)
      }
      root()
    }

  private val versionRandom = TRandom.wrap(global.versionRandom)

  override def toString = "Confluent"

  // ---- event ----

  final def indexMap: IndexMapHandler[T] = this

  @inline private def partialTree: Ancestor.Tree[D, Long] = global.partialTree

  final def newVersionId(implicit tx: T): Long = {
    implicit val dtx: D = durableTx(tx)
    val lin = global.versionLinear() + 1
    global.versionLinear() = lin
    var rnd = 0
    while ({
      rnd = versionRandom.nextInt()

      rnd == 0
    }) ()

    (rnd.toLong << 32) | (lin.toLong & 0xFFFFFFFFL)
  }

  final def newIdValue()(implicit tx: T): Int = {
    implicit val dtx: D = durableTx(tx)
    val res = global.idCnt() + 1
    global.idCnt() = res
    res
  }

  final def createTxn(dtx: D, inputAccess: Access[T], retroactive: Boolean, cursorCache: Cache[T],
                      systemTimeNanos: Long): T = {
    log(s"::::::: atomic - input access = $inputAccess${if (retroactive) " - retroactive" else ""} :::::::")
    wrapRegular(dtx, inputAccess, retroactive, cursorCache, systemTimeNanos)
  }

  final def readPath(in: DataInput): Access[T] = Path.read[T](in)

  final def newCursor()(implicit tx: T): Cursor[T, D] = newCursor(tx.inputAccess)

  final def newCursor(init: Access[T])(implicit tx: T): Cursor[T, D] = {
    implicit val dtx: D = durableTx(tx)
    implicit val s: ConfluentLike[T] { type D = self.D } = this
    Cursor[T, D](init)
  }

  final def readCursor(in: DataInput)(implicit tx: T): Cursor[T, D] = {
    implicit val dtx: D = durableTx(tx)
//    implicit val dAcc: Unit = ()
    implicit val s: ConfluentLike[T] { type D = self.D } = this  // this is to please the IntelliJ IDEA presentation compiler
    Cursor.read[T, D](in)
  }

  final def root[A](init: T => A)(implicit serializer: TSerializer[T, A]): TRef[T, A] =
    executeRoot { implicit tx =>
      rootBody(init)
    }

  final def rootJoin[A](init: T => A)
                       (implicit itx: TxnLike, serializer: TSerializer[T, A]): TRef[T, A] = {
    log("::::::: rootJoin :::::::")
    TxnExecutor.defaultAtomic { itx =>
      implicit val tx: T = wrapRoot(itx)
      rootBody(init)
    }
  }

  private def rootBody[A](init: T => A)
                         (implicit tx: T, serializer: TSerializer[T, A]): TRef[T, A] = {
    val (rootVar, _, _) = initRoot(init, _ => (), _ => ())
    rootVar
  }

  def cursorRoot[A, B](init: T => A)(result: T => A => B)
                      (implicit serializer: TSerializer[T, A]): (TRef[T, A], B) =
    executeRoot { implicit tx =>
      val (rootVar, rootVal, _) = initRoot(init, _ => (), _ => ())
      rootVar -> result(tx)(rootVal)
    }

  final def rootWithDurable[A, B](confInt: T => A)(durInit: D => B)
                                 (implicit aSer: TSerializer[T, A],
                                  bSer: TSerializer[D, B]): (TSource[T, A], B) =
    executeRoot { implicit tx =>
      implicit val dtx: D = durableTx(tx)
      val (_, confV, durV) = initRoot[A, B](confInt, { _ /* tx */ =>
        // read durable
        val did = global.durRootId
        // stm.DurableSurgery.read (durable)(did)(bSer.read(_, ()))
        durable.read(did)(bSer.readT(_)(dtx))
      }, { _ /* tx */ =>
        // create durable
        val _durV = durInit(dtx)
        val did = global.durRootId
        // stm.DurableSurgery.write(durable)(did)(bSer.write(_durV, _))
        durable.write(did)(bSer.write(_durV, _))
        _durV
      })
      tx.newHandle(confV) -> durV
    }

  private def executeRoot[A](fun: T => A): A = LTxn.atomic { itx =>
    log("::::::: root :::::::")
    val tx = wrapRoot(itx)
    fun(tx)
  }

  private def initRoot[A, B](initA: T => A, readB: T => B, initB: T => B)
                            (implicit tx: T, serA: TSerializer[T, A]): (TRef[T, A], A, B) = {
    val rootVar     = new RootVar[T, A](0, "Root") // serializer
    val rootPath    = tx.inputAccess
    val arrOpt      = varMap.getImmutable[Array[Byte]](0, tx)(rootPath, ByteArraySerializer)
    val (aVal, bVal) = arrOpt match {
      case Some(arr) =>
        val in      = DataInput(arr)
        // in theory, the read access should already be equal to inputAccess here...
        val aRead   = tx.withReadAccess(rootPath)(serA.readT(in)(tx))
        val bRead   = readB(tx)
        (aRead, bRead)

      case _ =>
        // implicit val dtx = durableTx(tx) // created on demand (now)
        writeNewTree(rootPath.index, 0)
        writePartialTreeVertex(partialTree.root)
        writeVersionInfo(rootPath.term)

        val aNew    = initA(tx)
        rootVar.setInit(aNew)
        val bNew    = initB(tx)
        (aNew, bNew)
    }
    (rootVar, aVal, bVal)
  }

  final def flushRoot(meldInfo: MeldInfo[T], newVersion: Boolean, caches: Vec[Cache[T]])
                     (implicit tx: T): Unit = {
    if (meldInfo.requiresNewTree) throw new IllegalStateException("Cannot meld in the root version")
    val outTerm = tx.inputAccess.term
    flush(outTerm, caches)
  }

  final def flushRegular(meldInfo: MeldInfo[T], newVersion: Boolean, caches: Vec[Cache[T]])
                        (implicit tx: T): Unit = {
    val newTree = meldInfo.requiresNewTree
    val outTerm = if (newTree) {
      if (tx.isRetroactive) throw new IllegalStateException("Cannot meld in a retroactive transaction")
      flushNewTree(meldInfo.outputLevel)
    } else {
      if (newVersion) flushOldTree() else tx.inputAccess.term
    }
    log(s"::::::: txn flush - ${if (newTree) "meld " else ""}term = ${outTerm.toInt} :::::::")
    if (newVersion) writeVersionInfo(outTerm)
    flush(outTerm, caches)
  }

  // writes the version info (using cookie `4`).
  private def writeVersionInfo(term: Long)(implicit tx: T): Unit = {
    val tint = term.toInt
    store.put { out =>
      out.writeByte(4)
      out.writeInt(tint)
    } { out =>
      val i = tx.info
      val m = i.message
      out.writeUTF(m)
      out.writeLong(i.timeStamp)
    }
  }

  /** Retrieves the version information for a given version term. */
  final def versionInfo(term: Long)(implicit tx: TxnLike): VersionInfo = {
    val vInt = term.toInt
    val opt = store.get { out =>
      out.writeByte(4)
      out.writeInt(vInt)
    } { in =>
      val m         = in.readUTF()
      val timeStamp = in.readLong()
      VersionInfo(m, timeStamp)
    }
    opt.getOrElse(sys.error(s"No version information stored for $vInt"))
  }

  final def versionUntil(access: Access[T], timeStamp: Long)(implicit tx: T): Access[T] = {
    @tailrec def loop(low: Int, high: Int): Int = {
      if (low <= high) {
        val index = ((high + low) >> 1) & ~1 // we want entry vertices, thus ensure index is even
        val thatTerm = access(index)
        val thatInfo = versionInfo(thatTerm)
        val thatTime = thatInfo.timeStamp
        if (thatTime == timeStamp) {
          index
        } else if (thatTime < timeStamp) {
          loop(index + 2, high)
        } else {
          loop(low, index - 2)
        }
      } else {
        -low - 1
      }
    }

    val sz = access.size
    if (sz % 2 != 0) throw new IllegalStateException(s"Provided path is index, not full terminating path $access")
    val idx = loop(0, sz - 1)
    // if idx is zero or positive, a time stamp was found, we can simply return
    // the appropriate prefix. if idx is -1, it means the query time is smaller
    // than the seminal version's time stamp; so in that case, return the
    // seminal path (`max(0, -1) + 1 == 1`)
    if (idx >= -1) {
      val index = access.take(math.max(0, idx) + 1)
      index :+ index.term
    } else {
      // otherwise, check if the last exit version is smaller than the query time,
      // and we return the full input access argument. otherwise, we calculate
      // the insertion index `idxP` which is an even number. the entry vertex
      // at that index would have a time stamp greater than the query time stamp,
      // and the entry vertex at that index minus 2 would have a time stamp less
      // than the query time step. therefore, we have to look at the time stamp
      // map for the entry vertex at that index minus 2, and find the ancestor
      // of the tree's exit vertex at idxP - 1.
      val idxP = -idx - 1
      if (idxP == sz && versionInfo(access.term).timeStamp <= timeStamp) {
        access
      } else {
        val (index, treeExit) = access.take(idxP).splitIndex
        val anc               = readTimeStampMap(index)
        val resOpt            = anc.nearestUntil(timeStamp = timeStamp, term = treeExit)
        val res               = resOpt.getOrElse(sys.error(s"No version info found for $index"))
        index :+ res._1
      }
    }
  }

  private def flush(outTerm: Long, caches: Vec[Cache[T]])(implicit tx: T): Unit =
    caches.foreach(_.flushCache(outTerm))

  private def flushOldTree()(implicit tx: T): Long = {
    implicit val dtx: D     = durableTx(tx)
    val childTerm           = newVersionId(tx)
    val (index, parentTerm) = tx.inputAccess.splitIndex
    val tree                = readIndexTree(index.term)
    val parent              = readTreeVertex(tree.tree, parentTerm)._1
    val retro               = tx.isRetroactive

    val child = if (retro) {
      tree.tree.insertRetroChild(parent, childTerm)
    } else {
      tree.tree.insertChild(parent, childTerm)
    }

    writeTreeVertex(tree, child)
    val tsMap               = readTimeStampMap(index)
    tsMap.add(childTerm, ()) // XXX TODO: more efficient would be to pass in `child` directly

    // ---- partial ----
    val pParent = readPartialTreeVertex(parentTerm)

    val pChild = if (retro)
      partialTree.insertRetroChild(pParent, childTerm)
    else
      partialTree.insertChild(pParent, childTerm)

    writePartialTreeVertex(pChild)

    childTerm
  }

  private def flushNewTree(level: Int)(implicit tx: T): Long = {
    implicit val dtx: D     = durableTx(tx)
    val term                = newVersionId(tx)
    val oldPath             = tx.inputAccess

    // ---- full ----
    writeNewTree(oldPath :+ term, level)

    // ---- partial ----
    val parentTerm = oldPath.term
    val pParent   = readPartialTreeVertex(parentTerm)
    val pChild    = partialTree.insertChild(pParent, term)
    writePartialTreeVertex(pChild)

    term
  }

  // do not make this final
  def close(): Unit = {
    store     .close()
    durable   .close()
  }

  def numRecords    (implicit tx: T): Int = store.numEntries
  def numUserRecords(implicit tx: T): Int = math.max(0, numRecords - 1)

  // ---- index tree handler ----

  private final class IndexMapImpl[A](protected val map: Ancestor.Map[D, Long, A])
    extends IndexMap[T, A] {

    override def toString = s"IndexMap($map)"

    def debugPrint(implicit tx: T): String = map.debugPrint // (durableTx(tx))

    def nearest(term: Long)(implicit tx: T): (Long, A) = {
      implicit val dtx: D = durableTx(tx)
      val v = readTreeVertex(map.full, term)._1
      val (v2, value) = map.nearest(v)
      (v2.version, value)
    }

    // XXX TODO: DRY
    def nearestOption(term: Long)(implicit tx: T): Option[(Long, A)] = {
      implicit val dtx: D = durableTx(tx)
      val v = readTreeVertex(map.full, term)._1
      map.nearestOption(v) map {
        case (v2, value) => (v2.version, value)
      }
    }

    // XXX TODO: DRY
    def nearestUntil(timeStamp: Long, term: Long)(implicit tx: T): Option[(Long, A)] = {
      implicit val dtx: D = durableTx(tx)
      val v = readTreeVertex(map.full, /* index, */ term)._1
      // timeStamp lies somewhere between the time stamp for the tree's root vertex and
      // the exit vertex given by the `term` argument (it may indeed be greater than
      // the time stamp of the `term` = exit vertex argument).
      // In order to find the correct entry, we need to find the nearest ancestor of
      // the vertex associated with `term`, i.e. `v`, for which the additional constraint
      // holds that the versionInfo stored with any candidate vertex is smaller than or equal
      // to the query `timeStamp`.
      //
      // the ancestor search may call the predicate function with any arbitrary z-coordinate,
      // even beyond versions that have already been created. thus, a pre-check is needed
      // before invoking `versionInfo`, so that only valid versions are checked. This is
      // achieved by the conditional `vInt <= maxVersionInt`.
      val maxVersionInt = term.toInt
      map.nearestWithFilter(v) { vInt =>
        if (vInt <= maxVersionInt) {
          // note: while versionInfo formally takes a `Long` term, it only really uses the 32-bit version int
          val info = versionInfo(vInt.toLong)(dtx) // any txn will do
          info.timeStamp <= timeStamp
        } else {
          false // query version higher than exit vertex, possibly an inexistent version!
        }
      } map {
        case (v2, value) => (v2.version, value)
      }
    }

    def add(term: Long, value: A)(implicit tx: T): Unit = {
      implicit val dtx: D = durableTx(tx)
      val v = readTreeVertex(map.full, term)._1
      map.add((v, value))
    }

    def write(out: DataOutput): Unit = map.write(out)
  }

  // writes the vertex information (pre- and post-order entries) of a full tree's leaf (using cookie `0`).
  private def writeTreeVertex(tree: IndexTree[D], v: Ancestor.Vertex[D, Long])(implicit tx: D): Unit =
    store.put { out =>
      out.writeByte(0)
      out.writeInt(v.version.toInt)
    } { out =>
      out./* PACKED */ writeInt(tree.term.toInt)
      out./* PACKED */ writeInt(tree.level     )
      tree.tree.vertexSerializer.write(v, out)
    }

  // creates a new index tree. this _writes_ the tree (using cookie `1`), as well as the root vertex.
  // it also creates and writes an empty index map for the tree, used for timeStamp search
  // (using cookie `5`).
  private def writeNewTree(index: Access[T], level: Int)(implicit tx: T): Unit = {
    val dtx   = durableTx(tx)
    val term  = index.term
    log(s"txn new tree ${term.toInt}")
    val tree  = Ancestor.newTree[D, Long](term)(dtx, implicitly[TSerializer[D, Long]], _.toInt)
    val it    = new IndexTreeImpl(tree, level)
    val vInt  = term.toInt
    store.put { out =>
      out.writeByte(1)
      out.writeInt(vInt)
    } {
      it.write
    }
    writeTreeVertex(it, tree.root)(dtx)

    val map = newIndexMap(tx, term, ())(index, TSerializer.Unit)
    store.put { out =>
      out.writeByte(5)
      out.writeInt(vInt)
    } {
      map.write
    }
  }

  def debugPrintIndex(index: Access[T])(implicit tx: T): String = readTimeStampMap(index).debugPrint

  // reads the index map maintained for full trees allowing time stamp search
  // (using cookie `5`).
  private def readTimeStampMap(index: Access[T])(implicit tx: T): IndexMap[T, Unit] = {
    val opt = store.get { out =>
      out.writeByte(5)
      out.writeInt(index.term.toInt)
    } { in =>
      readIndexMap[Unit](in, tx)(index, TSerializer.Unit)
    }
    opt.getOrElse(sys.error(s"No time stamp map found for $index"))
  }

  private def readIndexTree(term: Long)(implicit tx: D): IndexTree[D] = {
    val st = store
    st.get { out =>
      out.writeByte(1)
      out.writeInt(term.toInt)
    } { in =>
      val tree = Ancestor.readTree[D, Long](in)(tx, TSerializer.Long, _.toInt) // tx.durable
      val level = in./* PACKED */ readInt()
      new IndexTreeImpl(tree, level)
    } getOrElse {
      // `term` does not form a tree index. it may be a tree vertex, though. thus,
      // in this conditional step, we try to (partially) read `term` as vertex, thereby retrieving
      // the underlying tree index, and then retrying with that index (`term2`).
      st.get { out =>
        out.writeByte(0)
        out.writeInt(term.toInt)
      } { in =>
        val term2 = in./* PACKED */ readInt() // tree index!
        if (term2 == term) throw new IllegalStateException(s"Trying to access nonexistent tree ${term.toInt}")
        readIndexTree(term2.toLong)
      } getOrElse {
        throw new IllegalStateException(s"Trying to access nonexistent tree ${term.toInt}")
      }
    }
  }

  // reeds the vertex along with the tree level
  final def readTreeVertex(tree: Ancestor.Tree[D, Long], term: Long)
                          (implicit tx: D): (Ancestor.Vertex[D, Long], Int) = {
    store.get { out =>
      out.writeByte(0)
      out.writeInt(term.toInt)
    } { in =>
      in./* PACKED */ readInt() // tree index!
      val level   = in./* PACKED */ readInt()
      val v       = tree.vertexSerializer.readT(in)
      (v, level)
    } getOrElse sys.error(s"Trying to access nonexistent vertex ${term.toInt}")
  }

  // writes the partial tree leaf information, i.e. pre- and post-order entries (using cookie `3`).
  private def writePartialTreeVertex(v: Ancestor.Vertex[D, Long])(implicit tx: T): Unit =
    store.put { out =>
      out.writeByte(3)
      out.writeInt(v.version.toInt)
    } { out =>
      partialTree.vertexSerializer.write(v, out)
    }

  // ---- index map handler ----

  // creates a new index map for marked values and returns that map. it does not _write_ that map
  // anywhere.
  override final def newIndexMap[A](tx: T, rootTerm: Long, rootValue: A)
                          (implicit index: tx.Acc, serializer: ConstantSerializer[A]): IndexMap[T, A] = {
    implicit val dtx: D     = durableTx(tx)
    val tree                = readIndexTree(index.term)
    val full                = tree.tree
    val rootVertex          = if (rootTerm == tree.term) {
      full.root
    } else {
      readTreeVertex(full, rootTerm)._1
    }
    val map           = Ancestor.newMap[D, Long, A](full, rootVertex, rootValue)
    new IndexMapImpl[A](map)
  }

  override final def readIndexMap[A](in: DataInput, tx: T)
                           (implicit index: tx.Acc, serializer: ConstantSerializer[A]): IndexMap[T, A] = {
    implicit val dtx: D     = durableTx(tx)
    implicit val dAcc: Unit = ()
    val term                = index.term
    val tree                = readIndexTree(term)
    val map                 = Ancestor.readMap[D, Long, A](in, dtx, tree.tree)
    new IndexMapImpl[A](map)
  }

  // true is term1 is ancestor of term2
  def isAncestor(term1: Long, term2: Long)(implicit tx: T): Boolean = {
    implicit val dtx: D = durableTx(tx)
    if (term1 == term2) return true // same vertex
    if (term1.toInt > term2.toInt) return false // can't be an ancestor if newer

    val tree = readIndexTree(term1)
    if (tree.term == term1) return true // if term1 is the root then it must be ancestor of term2

    val v1 = readTreeVertex(tree.tree, term1)._1
    val v2 = readTreeVertex(tree.tree, term2)._1
    v1.isAncestorOf(v2)
  }

  // ---- partial map handler ----

  private final class PartialMapImpl[A](protected val map: Ancestor.Map[D, Long, A])
    extends IndexMap[T, A] {

    override def toString = s"PartialMap($map)"

    def debugPrint(implicit tx: T): String = map.debugPrint // (durableTx(tx))

    def nearest(term: Long)(implicit tx: T): (Long, A) = {
      implicit val dtx: D = durableTx(tx)
      val v = readPartialTreeVertex(term)
      val (v2, value) = map.nearest(v)
      (v2.version, value)
    }

    // XXX TODO: DRY
    def nearestOption(term: Long)(implicit tx: T): Option[(Long, A)] = {
      implicit val dtx: D = durableTx(tx)
      val v = readPartialTreeVertex(term)
      map.nearestOption(v).map {
        case (v2, value) => (v2.version, value)
      }
    }

    def nearestUntil(timeStamp: Long, term: Long)(implicit tx: T): Option[(Long, A)] = ???

    def add(term: Long, value: A)(implicit tx: T): Unit = {
      implicit val dtx: D = durableTx(tx)
      val v = readPartialTreeVertex(term)
      map.add((v, value))
    }

    def write(out: DataOutput): Unit = map.write(out)
  }

  private def readPartialTreeVertex(term: Long)(implicit tx: D): Ancestor.Vertex[D, Long] =
    store.get { out =>
      out.writeByte(3)
      out.writeInt(term.toInt)
    } { in =>
      partialTree.vertexSerializer.readT(in)
    } getOrElse {
      sys.error(s"Trying to access nonexistent vertex ${term.toInt}")
    }

  // ---- PartialMapHandler ----

  final def getIndexTreeTerm(term: Long)(implicit tx: T): Long = {
    implicit val dtx: D = durableTx(tx)
    readIndexTree(term).term
  }

  final def newPartialMap[A](rootValue: A)
                            (implicit tx: T, serializer: ConstantSerializer[A]): IndexMap[T, A] = {
    implicit val dtx: D = durableTx(tx)
    val map   = Ancestor.newMap[D, Long, A](partialTree, partialTree.root, rootValue)
    new PartialMapImpl[A](map)
  }

  final def readPartialMap[A](in: DataInput)
                             (implicit tx: T, serializer: ConstantSerializer[A]): IndexMap[T, A] = {
    implicit val dtx: D     = durableTx(tx)
    implicit val dAcc: Unit = ()
    val map   = Ancestor.readMap[D, Long, A](in, dtx, partialTree)
    new PartialMapImpl[A](map)
  }
}