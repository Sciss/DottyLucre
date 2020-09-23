package de.sciss.lucre.confluent

import java.io.File

import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Confluent, Durable, Var => LVar}
import de.sciss.serial.{DataInput, DataOutput, TFormat}

// XXX TODO should be a ScalaTest spec
object DoubleRootTest extends App {
  type S  = Confluent
  type T  = Confluent.Txn
  type D  = Durable  .Txn

  val dir       = File.createTempFile("double", "trouble")
  require(dir.delete())
  println(s"Directory: $dir")

  println("First iteration")
  iterate()
  println("Second iteration")
  iterate()

  class Data(val id: Ident[T], val vr: LVar[Int])

  implicit object DataFmt extends TFormat[T, Data] {
    override def write(v: Data, out: DataOutput): Unit = {
      v.id.write(out)
      v.vr.write(out)
    }

    override def readT(in: DataInput)(implicit tx: T): Data = {
      val id = tx.readId(in)
      val vr = id.readIntVar(in)
      new Data(id, vr)
    }
  }

  def iterate(): Unit = {
    val database              = BerkeleyDB.factory(dir, createIfNecessary = true)
    implicit val confluent: S = Confluent(database)
    // val durable             = confluent.durable

    //    val cursorAcc = durable.root { implicit tx =>
    //      println("New cursor")
    //      Cursor[S, D]()
    //    }
    //
    //    val cursor = durable.step { implicit tx => cursorAcc() }

    implicit val screwYou: TFormat[D, Cursor.Data[T, D]] = Cursor.Data.format[T, D] // "lovely" Scala type inference at its best
    val (varAcc, csrData) = confluent.rootWithDurable { implicit tx =>
      println("Init confluent")
      val id = tx.newId()
      val vr = id.newIntVar(33)
      new Data(id, vr)
    } { implicit tx =>
      println("Init durable")
      Cursor.Data[T, D]()
    }

    val cursor = Cursor.wrap(csrData)

    //    { tx => _ =>
    //      implicit val dtx = confluent.durableTx(tx)
    //      cursorAcc()
    //    }

    cursor.step { implicit tx =>
      val vr      = varAcc().vr
      val current = vr()
      vr()        = current + 1
      println(s"Recovered $current")
    }

    confluent.close()
  }
}