import annotation.tailrec

trait Heap[K, V] {

  def insert(key: K, value: V): Unit

  def extractMin(): V

  def decreaseKey(value: V, newKey: K): Unit

}

case class Entry[K, V](key: K, value: V)

class ArrayBufferHeap[K : Ordering, V] extends Heap[K, V] {
  // The heap itself, represented as an array
  private[this] val array = collection.mutable.ArrayBuffer.empty[Entry[K, V]]

  // A map from value to array index (needed to allow lookup by value in decreaseKey())
  private[this] val map = collection.mutable.Map.empty[V, Int]

  private[this] val keyOrdering = implicitly[Ordering[K]]
  
  def insert(key: K, value: V): Unit = {
    val entry = Entry(key, value)
    // Add entry to end of last level of tree (this may break heap property)
    val insertedIndex = append(entry)
    println(s"Inserted key ${entry.key} at index $insertedIndex")

    // Bubble-up until heap property is restored
    bubbleUp(insertedIndex, entry)
  }

  def extractMin(): V = {
    // Swap the root and the last leaf
    swap(0, array.size - 1)

    // Remove the old root, which is now at the end of the array
    val root = remove(array.size - 1)

    // Bubble-down the new root until heap property is restored
    if (!array.isEmpty)
      bubbleDown(0, array(0))

    // Return the old root
    root.value
  }

  def decreaseKey(value: V, newKey: K): Unit = {
    val index = map(value)
    val entry = array(index)
    require(keyOrdering.compare(newKey, entry.key) < 0, 
            s"New key ($newKey) must be smaller than existing key (${entry.key})")

    // update the entry's key
    val updatedEntry = entry.copy(key=newKey)
    array(index) = updatedEntry

    // Because key has got smaller, bubble-up the entry to its new home
    bubbleUp(index, updatedEntry)
  }
  
  override def toString = s"ArrayBufferHeap(${array.mkString(", ")})"

  private def append(entry: Entry[K, V]): Int = {
    array.append(entry)
    val insertedIndex = array.size - 1
    map(entry.value) = insertedIndex
    insertedIndex
  }

  private def remove(index: Int): Entry[K, V] = {
    val removedEntry = array.remove(index)
    map.remove(removedEntry.value)
    removedEntry
  }

  /**
   * Keep swapping the given element with its parent until the heap property is restored,
   * i.e. parent key <= child key.
   */
  @tailrec
  private def bubbleUp(index: Int, entry: Entry[K, V]): Unit = {
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
  private def bubbleDown(index: Int, entry: Entry[K, V]): Unit = {
    val childIndices = getChildIndices(index)
    if (heapPropertyViolated(entry.key, childIndices)) {
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

    // Swap array elements
    array(i) = b
    array(j) = a

    // Update indices map
    map(a.value) = j
    map(b.value) = i
  }
}
