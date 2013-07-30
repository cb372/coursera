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

  def extractMin(): V = {
    // Swap the root and the last leaf
    swap(0, array.size - 1)

    // Remove the old root, which is now at the end of the array
    val root = array.remove(array.size - 1)

    // Bubble-down the new root until heap property is restored
    if (!array.isEmpty)
      bubbleDown(0, array(0))

    // Return the old root
    root.value
  }

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

  /**
   * Keep swapping the given element with the smaller of its children
   * until the heap property is restored, i.e. parent key <= child key.
   */
  @tailrec
  private def bubbleDown(index: Int, entry: KeyValue[K, V]): Unit = {
    val childIndices = getChildIndices(index)
    if (!childIndices.isEmpty && heapPropertyViolated(entry.key, childIndices)) {
      val minChildIndex = childIndices.minBy(array(_).key)
      swap(index, minChildIndex)
      bubbleDown(minChildIndex, entry)
    }
  }

  private def heapPropertyViolated(parentKey: K, childIndices: Seq[Int]): Boolean = {
    val childSmallerThanParent = childIndices.find {
      case index => keyOrdering.compare(parentKey, array(index).key) > 0
    }
    childSmallerThanParent.isDefined
  }

  private def hasParent(index: Int) = index > 0
  private def getParentIndex(index: Int) = (Math.floor((index - 1) / 2)).toInt
  private def getChildIndices(index: Int) = 
    List(index * 2 + 1, index * 2 + 2).filter(_ < array.size)

  private def swap(i: Int, j: Int): Unit = {
    val a = array(i)
    val b = array(j)
    println(s"Swapping keys (${a.key}, ${b.key}) at indices ($i, $j)")
    array(i) = b
    array(j) = a
  }
}
