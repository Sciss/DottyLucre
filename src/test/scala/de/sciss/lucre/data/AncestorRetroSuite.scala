package de.sciss.lucre.data

import de.sciss.lucre.data.TotalOrder.Map
import de.sciss.lucre.geom.{IntCube, IntDistanceMeasure3D, IntPoint3D, IntPoint3DLike, IntSpace}
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Cursor, Durable, InMemory, Sys, TReader, TSerializer, TestUtil, Txn}
import de.sciss.serial.{DataInput, DataOutput, Writable}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec, Map => IMap, Set => ISet}
import scala.concurrent.stm.Ref

/*

  To run this test copy + paste the following into sbt:

  testOnly de.sciss.lucre.data.AncestorRetroSuite

*/
class AncestorRetroSuite extends AnyFeatureSpec with GivenWhenThen {
  val PARENT_LOOKUP             = true
  val MARKED_ANCESTOR           = true
  val NUM1                      = 5000 // 10000   // 4407 // 4407   // 10000 // 283 // 10000  // tree size in PARENT_LOOKUP
  val NUM2                      = 5500 // 11000   // tree size in MARKED_ANCESTOR // 100000    // 150000
  val MARKER_PERCENTAGE         = 0.3     // 0.3       // 0.5 // percentage of elements marked (0 to 1)
  val RETRO_CHILD_PERCENTAGE    = 0.1     // from those elements marked, amount which are inserted as retro-children (0 to 1)
  val RETRO_PARENT_PERCENTAGE   = 0.1     // from those elements marked, amount which are inserted as retro-parents (0 to 1)

  val INMEMORY                  = true
  val DATABASE                  = true

  val VERIFY_MARKTREE_CONTENTS  = false   // be careful to not enable this with large TREE_SIZE (> some 1000)
  val PRINT_DOT                 = false
  val PRINT_ORDERS              = false   // to print out the pre- and post-order lists (enable for debugging only)
  val HUNT_DOWN_ORDER_BUGS      = false

  def seed: Long                = 0L

  var verbose                   = false
  val DEBUG_LAST                = false   // if enabled, switches to verbosity for the last element in the sequence

  implicit val space: IntSpace.ThreeDim = IntSpace.ThreeDim

  if (INMEMORY) {
    withSys[InMemory, InMemory.Txn]("Mem", { () =>
      val s = InMemory()
      (s, s)
    }, (_, _) => ())
  }
  if (DATABASE) {
    withSys[Durable, Durable.Txn]("BDB", { () =>
      //      val dir = File.createTempFile("ancestor", "_database")
      //      dir.delete()
      //      dir.mkdir()
      //      println(dir.getAbsolutePath)
      val bdb = BerkeleyDB.tmp()
      val s = Durable(bdb)
      (s, s)
    }, {
      case (bdb, success) =>
        if (success) {
          //            val sz = bdb.step( bdb.numUserRecords( _ ))
          //            if( sz != 0 ) bdb.step( implicit tx => bdb.debugListUserRecords() ).foreach( println )
          //            assert( sz == 0, "Final DB user size should be 0, but is " + sz )
        }
        bdb.close()
    })
  }

  object FullVertexPre {
    implicit def serializer[T <: Txn[T]](implicit vertexReader: TReader[T, FullVertex[T]]): TSerializer[T, FullVertexPre[T]] =
      new SerImpl[T](vertexReader)

    private final class SerImpl[T <: Txn[T]](vertexReader: TReader[T, FullVertex[T]])
      extends TSerializer[T, FullVertexPre[T]] {
      def read(in: DataInput, tx: T)(implicit access: tx.Acc): FullVertexPre[T] = {
        val id  = in.readByte()
        val v   = vertexReader.read(in, tx)
        if (id == 0) v.preHeadKey else v.preTailKey
      }

      def write(v: FullVertexPre[T], out: DataOutput): Unit = v.write(out)
    }
  }

  sealed trait FullVertexPre[T <: Txn[T]] extends Writable with VertexSource[T, FullVertex[T]] {
    def order: FullPreOrder[T]

    def id: Int

    final def write(out: DataOutput): Unit = {
      out.writeByte(id)
      source.write(out)
    }

    override def equals(that: Any): Boolean =
      that.isInstanceOf[FullVertexPre[_]] && {
        val thatPre = that.asInstanceOf[FullVertexPre[_]]
        (id == thatPre.id) && (source == thatPre.source)
      }
  }

  final class FullVertexPreHead[T <: Txn[T]](val source: FullVertex[T])
    extends FullVertexPre[T] {

    def order: FullPreOrder[T] = source.pre

    def id = 0

    override def toString = s"$source<pre>"

    def debugString = s"$this@${source.pre.tag}"
  }

  final class FullVertexPreTail[T <: Txn[T]](val source: FullVertex[T])
    extends FullVertexPre[T] {

    def order: FullPreOrder[T] = source.preTail

    def id = 1

    override def toString = s"$source<pre-tail>"

    def debugString = s"$this@${source.preTail.tag}"
  }

  object FullTree {
    def apply[T <: Txn[T]]()(implicit tx: T): FullTree[T] = {
      implicit val pointView: (FullVertex[T] /*, T*/) => IntPoint3D = (p /*, tx*/) => p.toPoint // (tx)
      new FullTree[T] {
//        val system: T = tx.system
        val cube: IntCube = IntCube(0x40000000, 0x40000000, 0x40000000, 0x40000000)
        val t: SkipOctree[T, IntPoint3DLike, IntPoint3D, IntCube, FullVertex[T]] =
          SkipOctree.empty[T, IntPoint3DLike, IntPoint3D, IntCube, FullVertex[T]](cube)

        val orderObserver = new RelabelObserver[ T, FullVertex[ T ]]( "full", t )
        val preOrder : TotalOrder.Map[T, FullVertexPre[T]] = TotalOrder.Map.empty(orderObserver, _.order, 0)
        val postOrder: TotalOrder.Map[T, FullVertex   [T]] = TotalOrder.Map.empty(orderObserver, _.post, Int.MaxValue /* - 1 */)
        implicit lazy val vertexSer: TSerializer[T, FullVertex[T]] = new TSerializer[T, FullVertex[T]] {
          def write(v: FullVertex[T], out: DataOutput): Unit = v.write(out)

          def read(in: DataInput, tx: T)(implicit access: tx.Acc): FullVertex[T] = {
            new FullVertex[T] {
              val version: Int = in.readInt()
              val pre     : TotalOrder.Map.Entry[T, FullVertexPre[T]] = preOrder  .readEntry(in, tx)
              val post    : TotalOrder.Map.Entry[T, FullVertex   [T]] = postOrder .readEntry(in, tx)
              val preTail : TotalOrder.Map.Entry[T, FullVertexPre[T]] = preOrder  .readEntry(in, tx)
            }
          }
        }
        val root: FullVertex[T] = new FullVertex[T] {
          //               val pre: FullPreOrder[ S ]       = preOrder.root
          //               val post: FullPostOrder[ S ]     = postOrder.root // insertAfter( this, this )
          //               val preTail: FullPreOrder[ S ]   = preOrder.insertAfter( preHeadKey, preTailKey )
          val pre    : FullPreOrder [T] = preOrder .root
          val post   : FullPostOrder[T] = postOrder.root
          // insertAfter( this, this )
          val preTail: FullPreOrder [T] = preOrder.insert()
          val version = 0
          preOrder.placeAfter(preHeadKey, preTailKey) // preTailKey must come last
          //               override def toString = "full-root"
        }
        t += root
      }
    }
  }
  //   final class FullTree[ S <: Sys[ S ]] private (
  //      val t: SkipOctree[ S, Space.IntThreeDim, FullVertex[ S ]],
  //      val root: FullRootVertex[ S ],
  //      val preOrder: TotalOrder.Map[ S, FullVertexPre[ S ]],
  //      val postOrder: TotalOrder.Map[ S, FullVertex[ S ]],
  //      val vertexSer: Serializer[ FullVertex[ S ]]
  //   ) {

  sealed trait FullTree[T <: Txn[T]] {
//    def system: T

    def t: SkipOctree[T, IntPoint3DLike, IntPoint3D, IntCube, FullVertex[T]]

    def root: FullVertex[T]

    def preOrder : TotalOrder.Map[T, FullVertexPre[T]]
    def postOrder: TotalOrder.Map[T, FullVertex[T]]

    def vertexSer: TSerializer[T, FullVertex[T]]

    private type V = FullVertex[T]

    private val versionCnt = Ref(1)

    def nextVersion()(implicit tx: T): Int = {
      val res = versionCnt.get(tx.peer)
      versionCnt.set(res + 1)(tx.peer)
      res
    }

    def insertChild(parent: V)(implicit tx: T): V = {
      val v: FullVertex[T] = new FullVertex[T] {
        //            val pre     = preOrder.insertBefore( parent.preTailKey, preHeadKey )
        //            val preTail = preOrder.insertAfter( preHeadKey, preTailKey )
        //            val post    = postOrder.insertBefore( parent, this )
        val pre    : TotalOrder.Map.Entry[T, FullVertexPre[T]] = preOrder.insert()
        val preTail: TotalOrder.Map.Entry[T, FullVertexPre[T]] = preOrder.insert()
        val post   : TotalOrder.Map.Entry[T, FullVertex   [T]] = postOrder.insert()
        val version: Int = nextVersion()
        //if( version == 411 ) {
        //   println( "AHAAAA" )
        //}
        preOrder .placeBefore(parent.preTailKey, preHeadKey)
        postOrder.placeBefore(parent, this)
        preOrder .placeAfter (preHeadKey, preTailKey) // preTailKey must come last!
      }

      if (verbose) {
        val (chStr, pStr) = v.toPoint -> parent.toPoint
        println(s"Full ins. child $chStr with parent $pStr")
      }
      t.add(v)
      v
    }

    def insertRetroChild(parent: V)(implicit tx: T): V = {
      val v: FullVertex[T] = new FullVertex[T] {
        //            val pre     = preOrder.insertAfter( parent.preHeadKey, preHeadKey )
        //            val preTail = preOrder.insertBefore( parent.preTailKey, preTailKey )
        //            val post    = postOrder.insertBefore( parent, this )
        val pre    : TotalOrder.Map.Entry[T, FullVertexPre[T]]  = preOrder .insert()
        val preTail: TotalOrder.Map.Entry[T, FullVertexPre[T]]  = preOrder .insert()
        val post   : TotalOrder.Map.Entry[T, FullVertex   [T]]  = postOrder.insert()
        val version: Int = nextVersion()
        preOrder .placeAfter (parent.preHeadKey, preHeadKey)
        postOrder.placeBefore(parent, this)
        preOrder .placeBefore(parent.preTailKey, preTailKey)

        // preTailKey must come last
        override def toString = s"${super.toString}@r-ch"
      }

      t.add(v)
      v
    }

    def insertRetroParent(child: V)(implicit tx: T): V = {
      require(child != root)
      val v: FullVertex[T] = new FullVertex[T] {
        //            val pre     = preOrder.insertBefore( child.preHeadKey, preHeadKey )
        //            val preTail = preOrder.insertAfter( child.preTailKey, preTailKey )
        //            val post    = postOrder.insertAfter( child, this )
        val pre    : TotalOrder.Map.Entry[T, FullVertexPre[T]] = preOrder .insert()
        val preTail: TotalOrder.Map.Entry[T, FullVertexPre[T]] = preOrder .insert()
        val post   : TotalOrder.Map.Entry[T, FullVertex   [T]] = postOrder.insert()
        val version: Int = nextVersion()
        preOrder .placeBefore(child.preHeadKey, preHeadKey)
        postOrder.placeAfter (child, this)
        preOrder .placeAfter (child.preTailKey, preTailKey)

        // preTailKey must come last
        override def toString = s"${super.toString}@r-par"
      }

      t.add(v)
      v
    }

    //      def validate(): Unit = {
    ////         When( "the size of the vertices is queried from the quadtree" )
    ////         Then( "it should be equal to the number of observed labelings and relabelings" )
    ////         val qsz = t.system.step { implicit tx => t.size }
    ////         assert( qsz == preObserver.map.size,
    ////            "pre-observer size (" + preObserver.map.size + ") is different from quad size (" + qsz + ")" )
    ////         assert( qsz == postObserver.map.size,
    ////            "post-observer size (" + postObserver.map.size + ") is different from quad size (" + qsz + ")" )
    //      }
  }

  type FullPreOrder [T <: Txn[T]] = TotalOrder.Map.Entry[T, FullVertexPre[T]]
  type FullPostOrder[T <: Txn[T]] = TotalOrder.Map.Entry[T, FullVertex   [T]]

  sealed trait VertexSource[T <: Txn[T], V] {
    def source: V

    def debugString: String
  }

  sealed trait VertexLike[T <: Txn[T], Repr] extends Writable with VertexSource[T, Repr] {
    def version: Int

    def toPoint: IntPoint3D
  }

  sealed trait FullVertex[T <: Txn[T]] extends VertexLike[T, FullVertex[T]] {
    def source: FullVertex[T] = this

    final val preHeadKey = new FullVertexPreHead[T](this)
    final val preTailKey = new FullVertexPreTail[T](this)

    def pre    : FullPreOrder [T]
    def post   : FullPostOrder[T]
    def preTail: FullPreOrder [T]

    final def write(out: DataOutput): Unit = {
      out.writeInt(version)
      pre    .write(out)
      post   .write(out)
      preTail.write(out)
    }

    override def equals(that: Any): Boolean =
      that.isInstanceOf[FullVertex[_]] && (that.asInstanceOf[FullVertex[_]].version == version)

    //      final def x : Int = system.step { implicit tx => pre.tag }
    //      final def y : Int = system.step { implicit tx => post.tag }
    //      final def z : Int = version

    final def toPoint: IntPoint3D = IntPoint3D(pre.tag, post.tag, version)

    final def debugString = s"$toString<post>@${post.tag}"

    override def toString = s"Full($version)"
  }

  //   sealed trait FullRootVertex[ S <: Sys[ S ]] extends FullVertex[ S ] {
  ////      implicit def vertexSer : Serializer[ FullVertex[ S ]]
  ////      def preOrder  : TotalOrder.Map[ S, FullVertexPre[ S ]]
  ////      def postOrder : TotalOrder.Map[ S, FullVertex[ S ]]
  //   }

  final class RelabelObserver[T <: Txn[T], V <: VertexLike[T, V]](name: String,
                                                                  t: SkipOctree[T, IntPoint3DLike, IntPoint3D, IntCube, V])
    extends TotalOrder.Map.RelabelObserver[ T, VertexSource[ T, V ]] {
    def beforeRelabeling(/* v0s: VertexSource[ V ], */ iter: Iterator[VertexSource[T, V]])
                        (implicit tx: T): Unit = {
      if (verbose) println(s"RELABEL $name - begin")
      //         val v0 = v0s.source
      iter.foreach { vs =>
        val v = vs.source
        //            if( v ne v0 ) {
        if (verbose) {
          val str = try {
            vs.debugString
          } catch {
            case _: NullPointerException => "<null>"
          }
          println(s"RELABEL $name - $str")
        }
        //               assert( t.remove( v ), "When inserting " + v0 + ", the vertex " + v + " seems to have not been in the " + name + " tree" )

        // the nasty thing is, in the pre-order list the items appear twice
        // due to pre versus preTail. thus the items might get removed twice
        // here, too, and we cannot assert that t.remove( v ) == true
        t -= v
        //            }
      }
      if (verbose) println(s"RELABEL $name - end")
    }

    def afterRelabeling(/* v0s: VertexSource[ V ], */ iter: Iterator[VertexSource[T, V]])
                       (implicit tx: T): Unit = {
      if (verbose) println(s"RELABEL $name + begin")
      //         val v0 = v0s.source
      iter.foreach { vs =>
        val v = vs.source
        //            if( v ne v0 ) {
        if (verbose) {
          val str = try {
            vs.debugString
          } catch {
            case _: NullPointerException => "<null>"
          }
          println(s"RELABEL $name + $str")
        }
        //               assert( t.add( v ))

        // the nasty thing is, in the pre-order list the items appear twice
        // due to pre versus preTail. thus the items might get added twice
        // here, too, and we cannot assert that t.add( v ) == true
        t += v
        //            }
      }
      if (verbose) println(s"RELABEL $name + end")
    }
  }

  type MarkOrder[T <: Txn[T]] = TotalOrder.Map.Entry[T, MarkVertex[T]]

  //   object MarkVertex {
  //      implicit def reader[ S <: Sys[ S ]] : Reader[ MarkVertex[ S ]] = new ReaderImpl[ S ]
  //
  //      private final class ReaderImpl[ S <: Sys[ S ]] extends Reader[ MarkVertex[ S ]] {
  //         def read( in: DataInput ) : MarkVertex[ S ] = {
  //
  //         }
  //      }
  //   }
  sealed trait MarkVertex[T <: Txn[T]] extends VertexLike[T, MarkVertex[T]] {
    final def source: MarkVertex[T] = this

    def full: FullVertex[T]
    def pre : MarkOrder [T]
    def post: MarkOrder [T]

    //      final def x : Int = system.step { implicit tx => pre.tag }
    //      final def y : Int = system.step { implicit tx => post.tag }
    final def version: Int = full.version

    //      final def z : Int = full.version

    final def write(out: DataOutput): Unit = {
      full.write(out)
      pre .write(out)
      post.write(out)
    }

    final def toPoint: IntPoint3D = IntPoint3D(pre.tag, post.tag, version)

    override def toString = s"Mark($version)"

    def debugString: String = toString

    override def equals(that: Any): Boolean =
      that.isInstanceOf[MarkVertex[_]] && (that.asInstanceOf[MarkVertex[_]].version == version)
  }

  sealed trait MarkRootVertex[T <: Txn[T]] extends MarkVertex[T] {
    implicit def vertexSer: TSerializer[T, MarkVertex[T]]

    def preOrder : TotalOrder.Map[T, MarkVertex[T]]
    def postOrder: TotalOrder.Map[T, MarkVertex[T]]
  }

  object MarkTree {
    def apply[T <: Txn[T]](ft: FullTree[T])(implicit tx: T): MarkTree[T] = {
      implicit val pointView: (MarkVertex[T] /*, T*/) => IntPoint3D = (p /*, tx*/) => p.toPoint // (tx)
      lazy val orderObserver = new RelabelObserver[T, MarkVertex[T]]("mark", t)
      lazy val _vertexSer: TSerializer[T, MarkVertex[T]] = new TSerializer[T, MarkVertex[T]] {
        def write(v: MarkVertex[T], out: DataOutput): Unit = v.write(out)

        def read(in: DataInput, tx: T)(implicit access: tx.Acc): MarkVertex[T] = {
          new MarkVertex[T] {
            val full: FullVertex[T] = ft.vertexSer.read(in, tx)
            val pre : TotalOrder.Map.Entry[T, MarkVertex[T]] = root.preOrder .readEntry(in, tx)
            val post: TotalOrder.Map.Entry[T, MarkVertex[T]] = root.postOrder.readEntry(in, tx)
          }
        }
      }
      lazy val root: MarkRootVertex[T] = new MarkRootVertex[T] {
        implicit val vertexSer: TSerializer[T, MarkVertex[T]] = _vertexSer

        def full: FullVertex[T] = ft.root

        lazy val preOrder : TotalOrder.Map[T, MarkVertex[T]] = TotalOrder.Map.empty(orderObserver, _.pre)
        lazy val postOrder: TotalOrder.Map[T, MarkVertex[T]] = TotalOrder.Map.empty(orderObserver, _.post, rootTag = Int.MaxValue)
        //            lazy val pre: MarkOrder[ S ]  = preOrder.root
        //            lazy val post: MarkOrder[ S ] = postOrder.root.append( this )
        lazy val pre : MarkOrder[T] = preOrder.root
        lazy val post: MarkOrder[T] = postOrder.root // postOrder.insertAfter( root, this )
      }
      lazy val t = {
        implicit val keySer: TSerializer[T, MarkVertex[T]] = _vertexSer
        SkipOctree.empty[T, IntPoint3DLike, IntPoint3D, IntCube, MarkVertex[T]](ft.t.hyperCube)
      }
      t += root

      lazy val preList = {
        implicit val ord: scala.Ordering[MarkVertex[T]] = new scala.Ordering[MarkVertex[T]] {
          def compare(a: MarkVertex[T], b: MarkVertex[T]): Int = a.pre.compare(b.pre)
        }
        implicit val keySer: TSerializer[T, MarkVertex[T]] = _vertexSer
        val res = SkipList.Set.empty[T, MarkVertex[T]]
        res.add(root)
        res
      }

      lazy val postList = {
        implicit val ord: scala.Ordering[MarkVertex[T]] = new scala.Ordering[MarkVertex[T]] {
          def compare(a: MarkVertex[T], b: MarkVertex[T]): Int = a.post.compare(b.post)
        }
        implicit val keySer: TSerializer[T, MarkVertex[T]] = _vertexSer
        val res = SkipList.Set.empty[T, MarkVertex[T]]
        res.add(root)
        res
      }
      val mt = new MarkTree(ft, t, root, preList, postList)
      //         if( verbose ) mt.printInsertion( root )
      mt
    }
  }

  final class MarkTree[T <: Txn[T]] private(val ft: FullTree[T],
                                            val t: SkipOctree[T, IntPoint3DLike, IntPoint3D, IntCube, MarkVertex[T]],
                                            val root: MarkRootVertex[T], val preList: SkipList.Set[T, MarkVertex[T]],
                                            val postList: SkipList.Set[T, MarkVertex[T]]) {
    type V = MarkVertex[T]

//    def system: T = ft.system

    def printInsertion(vm: V)(implicit cursor: Cursor[T]): Unit = {
      val (mStr, fStr) = cursor.step {
        implicit tx => vm.toPoint -> vm.full.toPoint
      }
      println("Mark ins. node " + mStr + " with full " + fStr)
    }
  }

  class Config[T <: Txn[T]](val t: FullTree[T], val treeSeq: Vec[FullVertex[T]],
                            val parents: IMap[FullVertex[T], FullVertex[T]])

  def withSys[S <: Sys, T <: Txn[T]](sysName: String, sysCreator: () => (S, Cursor[T]),
                                          sysCleanUp: (S, Boolean) => Unit): Unit = {
    def randomlyFilledTree(n: Int)(implicit cursor: Cursor[T]): Config[T] = {
      Given("a randomly filled tree, corresponding node orders and their quadtree")
      val (t, treeSeq, parents) = cursor.step { implicit tx =>
        val tr        = FullTree[T]()
        val rnd       = new util.Random(seed)
        var treeSeq   = Vec[FullVertex[T]](tr.root)
        var parents   = IMap.empty[FullVertex[T], FullVertex[T]]
        var children  = IMap.empty[FullVertex[T], ISet[FullVertex[T]]]

        for (i <- 1 to n) {
          if (DEBUG_LAST && i == n) {
            verbose = true
          }
          //            try {
          val refIdx = rnd.nextInt(i)
          val ref = treeSeq(refIdx)
          val retro = if (refIdx > 0) rnd.nextDouble() else 1.1 // no retro stuff with root!
          if (retro <= RETRO_CHILD_PERCENTAGE) {
            if (verbose) println(s"v$i is retro child to $refIdx")
            val child = tr.insertRetroChild(ref /*, i */)
            treeSeq :+= child
            parents += child -> ref
            val oldChildren = children.getOrElse(ref, Set.empty)
            children += ref -> Set(child) // only child (overwrite previous entries for parent)
            oldChildren.foreach {
              c2 => parents += c2 -> child
            } // update parent for old children
            children += child -> oldChildren
          } else if (retro <= (RETRO_CHILD_PERCENTAGE + RETRO_PARENT_PERCENTAGE)) {
            if (verbose) println(s"v$i is retro parent to $refIdx")
            val parent = tr.insertRetroParent(ref /*, i */)
            treeSeq :+= parent
            val oldParentO = parents.get(ref)
            parents += ref -> parent // overwrites previous entry
            oldParentO.foreach(op => parents += parent -> op)
            children += parent -> Set(ref)
            oldParentO.foreach {
              oldParent =>
                children += oldParent -> (children.getOrElse(oldParent, Set.empty) - ref + parent) // replace child
            }
          } else {
            // regular child
            if (verbose) println(s"v$i is child to $refIdx")
            val child = tr.insertChild(ref /*, i */)
            treeSeq :+= child
            parents += child -> ref
            children += ref -> (children.getOrElse(ref, Set.empty) + child)
          }
          //if( verbose ) printPrePost( t, treeSeq )

          //            } catch {
          //               case e =>
          //                  println( "(for i = " + i + ")" )
          //                  throw e
          //            }

          if (HUNT_DOWN_ORDER_BUGS) {
            treeSeq.foreach {
              v =>
                v.pre.validate("pre " + i)
                v.post.validate("post " + i)
                v.preTail.validate("preTail " + i)
            }
          }

        }
        (tr, treeSeq, parents)
      }
      new Config[T](t, treeSeq, parents)
    }

    //   private def printPrePost( t: FullTree, treeSeq: IndexedSeq[ FullTree#Vertex ]) {
    //      println( " PRE ORDER: " + t.r.head.tagList.map( pre => treeSeq.find( _.pre.tag == pre )).collect({ case Some( v ) => v.version }).mkString( ", " ))
    //      println( "POST ORDER: " + t.postOrder.head.tagList.map( post => treeSeq.find( _.post.tag == post ).get.version ).mkString( ", " ))
    //   }

    def scenarioWithTime(name: String, descr: String)(body: => Unit): Unit =
      Scenario(descr) {
        val t1 = System.currentTimeMillis()
        body
        val t2 = System.currentTimeMillis()
        println(s"For $name ($sysName) the tests took ${TestUtil.formatSeconds((t2 - t1) * 0.001)}")
      }

    if (PARENT_LOOKUP) {
      Feature(s"Tree parent node lookup should be possible in a $sysName octree representing pre-order, post-order and version") {
        info("The vertices of a tree are represented by their positions")
        info("in the tree's pre- and post-order traversals (as total orders), plus an incremental version.")
        info("NN search is possible with these orders representing")
        info("the x, y and z coordinates of an octree.")

        scenarioWithTime("Parent-Lookup", "Verifying parent node lookup") {
          val (system, _cursor) = sysCreator()
          implicit val cursor: Cursor[T] = _cursor
          var success = false
          try {
            val config = randomlyFilledTree(NUM1)
            import config._
            //         val (t, treeSeq, parents) = randomlyFilledTree()
            //                  t.validate()

            // If a valid ancestor is defined by being left of the query in
            // the pre-order, and right of the query in the post-order,
            // and by having a version smaller than or equal to query version,
            // Then, Given that the pre-order is horizontally stored,
            // and the post-order is vertically stored, and the version is stored in the z-axis,
            // we can express this by constraining the search to the orthant
            // index binary 010 = 2. From the candidates we need
            // to find the one that is closest in the pre- or post-order. This
            // is expressed by a XY chebychev distance measure.
            When("each vertex is asked for its parent node through NN search in the quadtree")
            Then("the results should be identical to an independently maintained map")
            val metric = IntDistanceMeasure3D.chebyshevXY.orthant(2)
            // val metric = IntDistanceMeasure3D.vehsybehcXY.orthant( 2 )

            //         if( verbose ) printPrePost( t, treeSeq )

            @tailrec def testChild(version: Int, child: FullVertex[T]): Unit = {
              parents.get(child) match {
                case None =>

                case Some(parent) if parent.version <= version =>
                  val found: Option[FullVertex[T]] = cursor.step { implicit tx =>
                    val p0    = child.toPoint
                    //                     val point = IntPoint3D( child.x - 1, child.y + 1, child.version ) // make sure we skip the child itself
                    val point = p0.copy(x = p0.x - 1, y = p0.y + 1)
                    val f     = t.t.nearestNeighborOption(point, metric)
                    f
                  }
                  assert(found.contains(parent), s"For child $child, found $found instead of $parent")

                case Some(parent) =>
                  testChild(version, parent) // skip too new retro versions
              }
            }

            treeSeq.foreach {
              child => testChild(child.version, child)
            }
            success = true

          } finally {
            sysCleanUp(system, success)
          }
        }
      }
    }

    if (MARKED_ANCESTOR) {
      Feature(s"Marked ancestor lookup should be possible through isomorphic mapping between two $sysName octrees") {
        info("Two trees are now maintained (as quadtrees with pre/post order coordinates).")
        info("One tree represents the full version tree, the other a subtree representing markers.")
        info("Marked ancestor lookup is performed by translating a coordinate from the")
        info("full tree into the marker tree, followed by NN search.")

        scenarioWithTime("Marked-Ancestors", "Verifying marked ancestor lookup") {
          val (system, _cursor) = sysCreator()
          implicit val cursor: Cursor[T] = _cursor
          var success = false
          try {
            Given("a randomly filled tree, corresponding node orders and their octree")
            Given("a random marking of a subset of the vertices")

            val config: Config[T] = randomlyFilledTree(NUM2)
            import config._
            if (DEBUG_LAST) verbose = false
            val tm  = cursor.step { implicit tx => MarkTree(t) }
            val rnd = new util.Random(seed)

            var markSet = Set(0)

            treeSeq.zipWithIndex.drop(1).foreach {
              case (child, i) =>
                if (DEBUG_LAST && i == NUM2 - 1) verbose = true
                if (rnd.nextDouble() < MARKER_PERCENTAGE) {
                  cursor.step { implicit tx =>
                    if (verbose) println(":: mark insert for full " + child.toPoint)
                    val cfPre = child.pre
                    val (cmPreN, cmPreCmp) = tm.preList.isomorphicQuery { (that: MarkVertex[T]) =>
                      val res = cfPre.compare(that.full.pre)
                      if (verbose) println(":: mark insert pre :: compare to m=" + that.toPoint + ", f=" + that.full.toPoint + " -> " + res)
                      res
                    }
                    val cfPost = child.post
                    val (cmPostN, cmPostCmp) = tm.postList.isomorphicQuery { (that: MarkVertex[T]) =>
                      val res = cfPost.compare(that.full.post)
                      if (verbose) println(":: mark insert post :: compare to m=" + that.toPoint + ", f=" + that.full.toPoint + " -> " + res)
                      res
                    }
                    if (verbose) println(":: mark insert pre " + (if (cmPreCmp <= 0) "before" else "after") + " " + cmPreN.toPoint)
                    if (verbose) println(":: mark insert post " + (if (cmPostCmp <= 0) "before" else "after") + " " + cmPostN.toPoint)
                    val vm: MarkVertex[T] = new MarkVertex[T] {
                      val pre : Map.Entry[T, MarkVertex[T]] = tm.root.preOrder .insert()
                      val post: Map.Entry[T, MarkVertex[T]] = tm.root.postOrder.insert()
                      val full: FullVertex[T] = child
                      if (cmPreCmp <= 0) {
                        tm.root.preOrder.placeBefore(cmPreN, this) // cmPreN.prepend(  this )
                      } else {
                        tm.root.preOrder.placeAfter(cmPreN, this) // cmPreN.append(  this )
                      }
                      if (cmPostCmp <= 0) {
                        tm.root.postOrder.placeBefore(cmPostN, this) // cmPostN.prepend( this )
                      } else {
                        tm.root.postOrder.placeAfter(cmPostN, this) // cmPostN.append( this )
                      }
                    }

                    if (verbose) tm.printInsertion(vm)

                    tm.t.add(vm)
                    tm.preList .add(vm)
                    tm.postList.add(vm)
                    markSet += i
                  }
                }
            }

            When("full and marked tree are decomposed into pre and post order traversals")

            val preVals   = cursor.step { implicit tx => treeSeq.sortBy(_.pre .tag).map(_.version) }
            val postVals  = cursor.step { implicit tx => treeSeq.sortBy(_.post.tag).map(_.version) }
            val mPreSeq   = cursor.step { implicit tx => tm.preList.toIndexedSeq }
            val mPreVals  = mPreSeq.map(_.version) // ( t => t.value.version preTagValueMap( t ))
            val mPostSeq  = cursor.step { implicit tx => tm.postList.toIndexedSeq }
            val mPostVals = mPostSeq.map(_.version) // ( t => postTagValueMap( t ))

            if (PRINT_ORDERS) {
              println(preVals  .mkString(" pre full: ", ", ", ""))
              println(postVals .mkString("post full: ", ", ", ""))
              println(mPreVals .mkString(" pre mark: ", ", ", ""))
              println(mPostVals.mkString("post mark: ", ", ", ""))
            }

            Then("the order of the marked vertices is isomorphic to their counterparts in the full lists")
            assert(preVals .intersect(mPreVals ) == mPreVals , preVals .take(20).toString + " versus " + mPreVals.take(20))
            assert(postVals.intersect(mPostVals) == mPostVals, postVals.take(20).toString + " versus " + mPreVals.take(20))

            if (PRINT_DOT) {
              val sb = new StringBuilder()
              sb.append("digraph Tree {\n")
              treeSeq.foreach { v =>
                val id = v.version
                sb.append("  " + id.toString)
                sb.append("\n")
              }
              parents.foreach {
                case (child, parent) =>
                  sb.append("  " + parent.version.toString + " -> " + child.version.toString + "\n")
              }
              sb.append("}\n")
              println(sb.toString())
            }

            When("each vertex is asked for its nearest marked ancestor through mapping to the marked quadtree and NN search")
            Then("the results should be identical to those obtained from independent brute force")

            //println( "\n-----TREE-----" ); treeSeq.foreach( println )
            //println( "\n-----MARK-----" ); markSet.foreach( println )
            //println( "\n-----PARE-----" ); parents.foreach( println )
            //println()
            //verbose = false
            if (VERIFY_MARKTREE_CONTENTS) {
              val markPt = cursor.step {
                implicit tx => tm.t.toIndexedSeq.map(_.toPoint)
              }
              val obsPre  = markPt.sortBy(_.x).map(_.z)
              val obsPost = markPt.sortBy(_.y).map(_.z)
              //            println( "managed mark-pre:" )
              //            println( mPreVals.mkString( ", " ))
              //            println( "octree mark-pre:" )
              //            println( obsPre.mkString( ", " ))
              assert(obsPre  == mPreVals )
              assert(obsPost == mPostVals)
            }

            val metric = IntDistanceMeasure3D.chebyshevXY.orthant(2)
            // val metric = IntDistanceMeasure3D.vehsybehcXY.orthant( 2 )

            if (DEBUG_LAST) verbose = false
            treeSeq.zipWithIndex.foreach {
              case (child, i) =>
                if (DEBUG_LAST && i == NUM2 - 1) verbose = true
                val (found, parent, point) = cursor.step { implicit tx =>
                  val cfPre = child.pre
                  val (preIso, preIsoCmp) = tm.preList.isomorphicQuery { (that: MarkVertex[T]) =>
                    val res = cfPre.compare(that.full.pre)
                    if (verbose) println(":: mark find pre :: compare to m=" + that.toPoint + ", f=" + that.full.toPoint + " -> " + res)
                    res
                  }
                  val cfPost = child.post
                  val (postIso, postIsoCmp) = tm.postList.isomorphicQuery { (that: MarkVertex[T]) =>
                    val res = cfPost.compare(that.full.post)
                    if (verbose) println(":: mark find post :: compare to m=" + that.toPoint + ", f=" + that.full.toPoint + " -> " + res)
                    res
                  }

                  // condition for ancestor candidates: <= iso-pre, >= iso-post, <= version
                  // thus: we need to check the comparison result of the iso-search
                  // - if the pre-comp is -1, we need to make the iso-mapped x (pre) one smaller
                  // - if the post-comp is 1, we need to make the iso-mapped y (post) one larger
                  // - if the version-comp is -1, we need to make iso-mapped z (version) one smaller
                  // (although, we can skip the last two steps, as the false positive is already ruled out by step 1)
                  // (there is no iso-mapping for the version)
                  //
                  // We can also shortcut. pre-comp == 0 implies post-comp == 0, since no two
                  // vertices can have the same positions in the orders.
                  // Thus, When pre-comp == 0 is detected, we already found our ancestor!

                  if (verbose) println(":: mark find pre " + (if (preIsoCmp <= 0) "before" else "after") + " " + preIso.toPoint)
                  if (verbose) println(":: mark find post " + (if (postIsoCmp <= 0) "before" else "after") + " " + postIso.toPoint)

                  val par = {
                    var p = child
                    while (p.version > child.version || !markSet.contains(p.version)) {
                      p = parents(p)
                    }
                    p.version
                  }
                  if (preIsoCmp == 0) {
                    assert(postIsoCmp == 0)
                    val _pnt = IntPoint3D(preIso.pre.tag, postIso.post.tag, child.version)
                    val _f   = Some(preIso.version)
                    (_f, par, _pnt)
                  } else {
                    val x    = if (preIsoCmp < 0) preIso.pre.tag - 1 else preIso.pre.tag
                    val y    = if (postIsoCmp > 0) postIso.post.tag + 1 else postIso.post.tag
                    val _pnt = IntPoint3D(x, y, child.version)
                    val _f   = tm.t.nearestNeighborOption(_pnt, metric).map(_.version)
                    (_f, par, _pnt)
                  }
                }
                assert(found.contains(parent), s"For child $child (iso $point), found ${found.orNull} instead of $parent")
            }
            success = true

          } finally {
            sysCleanUp(system, success)
          }
        }
      }
    }
  }
}