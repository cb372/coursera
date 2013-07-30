import annotation.tailrec

case class KeyValue[K, V](key: K, value: V)

trait Heap[K, V] {

  def insert(entry: KeyValue[K, V]): Unit

  def extractMin(): V

  def decreaseKey(value: V, newKey: K): Unit

}

class ArrayBufferHeap[K : Ordering, V] extends Heap[K, V] {
  private[this] val array = collection.mutable.ArrayBuffer.empty[KeyValue[K, V]]
  private[this] val keyOrdering = implicitly[Ordering[K]]
  
  def insert(entry: KeyValue[K, V]): Unit = {
    // Add entry to end of last level of tree (this may break heap property)
    array.append(entry)
    val insertedIndex = array.size - 1
    println(s"Inserted key ${entry.key} at index $insertedIndex")

    // Bubble-up until heap property is restored
    bubbleUp(insertedIndex, entry)
  }

  def extractMin(): V = ???

  def decreaseKey(value: V, newKey: K): Unit = ???
  
  override def toString = s"ArrayBufferHeap(${array.mkString(", ")})"

  /**
   * Keep swapping the given element with its parent until the heap property is restored,
   * i.e. parent key <= child key.
   */
  @tailrec
  private def bubbleUp(index: Int, entry: KeyValue[K, V]): Unit = {
    if (hasParent(index)) {
      val parentIndex = getParentIndex(index)
      val parent = array(parentIndex)
      if (keyOrdering.compare(parent.key, entry.key) > 0) {
        swap(index, parentIndex)
        bubbleUp(parentIndex, entry)
      }
    }
  }

  private def hasParent(index: Int) = index > 0
  private def getParentIndex(index: Int) = (Math.floor((index - 1) / 2)).toInt

  private def swap(i: Int, j: Int): Unit = {
    val a = array(i)
    val b = array(j)
    println(s"Swapping keys (${a.key}, ${b.key}) at indices ($i, $j)")
    array(i) = b
    array(j) = a
  }
}
