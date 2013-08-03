import annotation.tailrec

/*
 * A heap implementation suitable for use in Dijkstra's algorithm
 */

/** A heap entry (key-value pair) */
case class Entry[K, V](key: K, value: V)

/** A Heap, a.k.a. priority queue */
trait Heap[K, V] {

  /** Insert the given key-value pair into the heap */
  def insert(key: K, value: V): Unit

  /** Remove the smallest element from the heap (sorted by key) */
  def extractMin(): Entry[K, V]

  /** 
   * Update the key for the given value.
   * If the given key is >= than the current one,
   * no work is done and the heap is left unchanged.
   */
  def decreaseKey(value: V, newKey: K): Unit

}

/** A heap implementation based on ArrayBuffer */
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

    // Bubble-up until heap property is restored
    bubbleUp(insertedIndex, entry)
  }

  def extractMin(): Entry[K, V] = {
    // Swap the root and the last leaf
    swap(0, array.size - 1)

    // Remove the old root, which is now at the end of the array
    val root = remove(array.size - 1)

    // Bubble-down the new root until heap property is restored
    if (!array.isEmpty)
      bubbleDown(0, array(0))

    // Return the old root
    root
  }

  def decreaseKey(value: V, newKey: K): Unit = {
    val index = map(value)
    val entry = array(index)
    
    // Don't do anything if the key has not actually decreased
    if (keyOrdering.compare(newKey, entry.key) >= 0)
      return

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

    // Swap array elements
    array(i) = b
    array(j) = a

    // Update indices map
    map(a.value) = j
    map(b.value) = i
  }
}

type Id = Int
type Length = Int
case class Edge(tail: Id, head: Id, length: Length)
case class Vertex(id: Id, edgesOut: Seq[Edge])
type Graph = Map[Id, Vertex]

/** Initialize the heap of unprocessed vertices */
def buildUnprocessedHeap(graph: Graph, sourceVertex: Vertex): Heap[Length, Id] = {
  val unknownDistance = Int.MaxValue
  val unprocessed: Heap[Length, Id] = new ArrayBufferHeap[Length, Id]

  // Add source vertex's adjacent nodes to heap
  for (e <- sourceVertex.edgesOut) { unprocessed.insert(e.length, e.head) }

  // Add all other vertices to heap with unknown distance
  val unknown = graph.keys.filterNot{ i => 
       i == sourceVertex.id || sourceVertex.edgesOut.find(_.head == i).isDefined 
  }
  for (i <- unknown) { unprocessed.insert(unknownDistance, i) }

  unprocessed
}

case class StepResult(processed: Set[Id], shortestPathLengths: Map[Id, Length])
/** Perform one step of Dijkstra's algorithm, i.e. add one vertex to the processed set. */
def dijkstraStep(graph: Graph,
                 processed: Set[Id], 
                 shortestPathLengths: Map[Id, Length], 
                 unprocessed: Heap[Length, Id]): StepResult = {
  val nextHeapEntry = unprocessed.extractMin
  val shortestPath = nextHeapEntry.key
  val vertexId = nextHeapEntry.value

  // Update shortest path distances for adjacent vertices in V-X
  for (edge <- graph(vertexId).edgesOut if !(processed contains edge.head)) {
    unprocessed.decreaseKey(edge.head, shortestPath + edge.length)
  }

  StepResult(processed + vertexId, shortestPathLengths + (vertexId -> shortestPath))
}

/** 
 * Dijkstra's algorithm to compute shortest paths from the given source vertex to every other vertex
 */
def dijkstra(graph: Graph, sourceVertexId: Id): Map[Id, Length] = {
  var processed: Set[Id] = Set(sourceVertexId)
  var shortestPathLengths: Map[Id, Length] = Map(sourceVertexId -> 0)
  val unprocessed: Heap[Length, Id] = buildUnprocessedHeap(graph: Graph, graph(sourceVertexId))

  while (processed.size < graph.size) {
    val stepResult = dijkstraStep(graph, processed, shortestPathLengths, unprocessed)
    processed = stepResult.processed
    shortestPathLengths = stepResult.shortestPathLengths
  }

  shortestPathLengths
}

/* Input munging */
val input: Iterator[(Id, List[(Id, Length)])] = {
  io.Source.fromFile("dijkstraData.txt")
    .getLines
    .map(_.split("\t").toList)
    .map { 
      case x :: xs => {
        val id = x.toInt
        val edges = xs.map(_.split(",").toList).map {
          case x::y::Nil => (x.toInt, y.toInt)
        }
        (id, edges)
      }
    }
}

def buildGraph(input: Iterator[(Int, List[(Int, Int)])]): Graph = {
  input.foldLeft(Map[Id, Vertex]()){
    case (vertices, (id, edges)) => {
      vertices + (id -> Vertex(id, edges.map { case (headId, length) => Edge(id, headId, length)}))
    }
  }
}

val graph: Graph = buildGraph(input)
val sourceVertexId = 1
val shortestPaths = dijkstra(graph, sourceVertexId)

// Print out the shortest paths to the given vertices
val result = List(7,37,59,82,99,115,133,165,188,197).map(shortestPaths(_))
println(result.mkString(","))

