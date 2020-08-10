import de.sciss.lucre.data.SkipList
import de.sciss.lucre.stm.{TxSerializer, Txn}

abstract class SkipListTest[T <: Txn[T]](tx0: T) {
  private val setH = {
    implicit val tx: T = tx0
    val s = SkipList.Set.empty[T, Int]
    implicit val ser: TxSerializer[T, SkipList.Set[T, Int]] = SkipList.Set.serializer()
    tx.newHandle(s)
  }
  
  private def set(implicit tx: T): SkipList.Set[T, Int] = setH()
  
  def add(i: Int)(implicit tx: T): Unit =
    set.add(i)
  
  def remove(i: Int)(implicit tx: T): Boolean =
    set.remove(i)
  
  def contains(i: Int)(implicit tx: T): Boolean = 
    set.contains(i)
}
