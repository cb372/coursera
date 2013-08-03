import annotation.tailrec

/*
 * A heap implementation suitable for use in Dijkstra's algorithm
 */

/** A heap entry (key-value pair) */
case class Entry[K, V](key: K, value: V)

/** A Heap, a.k.a. priority queue */
trait Heap[K, V] {

  /** Insert the given key-value pair into the heap */
  def insert(key: K, value: V): Heap[K, V]

  /** Remove the smallest element from the heap (sorted by key) */
  def extractMin(): (Entry[K, V], Heap[K, V])

  /** 
   * Update the key for the given value.
   * If the given key is >= than the current one,
   * no work is done and the heap is left unchanged.
   */
  def decreaseKey(value: V, newKey: K): Heap[K, V]

}

/** An immutable heap implementation based on Vector */
class VectorHeap[K : Ordering, V](vector: Vector[Entry[K, V]], map: Map[V, Int]) extends Heap[K, V] {
  private[this] val keyOrdering = implicitly[Ordering[K]]

  private type State = (Vector[Entry[K,V]], Map[V, Int])
  private val state = (vector, map)
  private def updated(state: State): VectorHeap[K, V] = new VectorHeap(state._1, state._2)

  def insert(key: K, value: V): Heap[K, V] = {
    val entry = Entry(key, value)
    // Add entry to end of last level of tree (this may break heap property)
    val (insertedIndex, stateAfterAppend) = append(entry, state)

    // Bubble-up until heap property is restored
    val stateAfterBubbling = bubbleUp(insertedIndex, entry, stateAfterAppend)

    updated(stateAfterBubbling)
  }

  def extractMin(): (Entry[K, V], Heap[K, V]) = {
    // Swap the root and the last leaf
    val stateAfterSwap = swap(0, vector.size - 1, state)

    // Remove the old root, which is now at the end of the array
    val (root, stateAfterRemove) = remove(vector.size - 1, stateAfterSwap)

    // Bubble-down the new root until heap property is restored
    val stateAfterBubbleDown = {
      val oldVector = stateAfterRemove._1
      if (!oldVector.isEmpty)
        bubbleDown(0, oldVector(0), stateAfterRemove)
      else
        stateAfterRemove
    }

    // Return the old root
    (root, updated(stateAfterBubbleDown))
  }

  def decreaseKey(value: V, newKey: K): Heap[K, V] = {
    val index = map(value)
    val entry = vector(index)
    
    // Don't do anything if the key has not actually decreased
    if (keyOrdering.compare(newKey, entry.key) >= 0)
      return this

    // update the entry's key
    val updatedEntry = entry.copy(key=newKey)
    val updatedVector = vector.updated(index, updatedEntry)

    // Because key has got smaller, bubble-up the entry to its new home
    val stateAfterBubbleUp = bubbleUp(index, updatedEntry, (updatedVector, map))

    updated(stateAfterBubbleUp)
  }
  
  override def toString = s"VectorHeap(${vector.mkString(", ")})"

  private def append(entry: Entry[K, V], state: State): (Int, State) = {
    val (oldVector, oldMap) = state
    (oldVector.size, ((oldVector :+ entry), oldMap + (entry.value -> vector.size)))
  }

  private def remove(index: Int, state: State): (Entry[K, V], State) = {
    val (oldVector, oldMap) = state
    val removedEntry = oldVector(index)
    (removedEntry, (oldVector.patch(index, Nil, 1), oldMap - removedEntry.value))
  }

  /**
   * Keep swapping the given element with its parent until the heap property is restored,
   * i.e. parent key <= child key.
   */
  @tailrec
  private def bubbleUp(index: Int, entry: Entry[K, V], state: State): State = {
    if (hasParent(index)) {
      val parentIndex = getParentIndex(index)
      val parent = state._1(parentIndex)
      if (keyOrdering.compare(parent.key, entry.key) > 0) {
        return bubbleUp(parentIndex, entry, swap(index, parentIndex, state))
      } else {
        state
      }
    } else {
      state
    }
  }

  /**
   * Keep swapping the given element with the smaller of its children
   * until the heap property is restored, i.e. parent key <= child key.
   */
  @tailrec
  private def bubbleDown(index: Int, entry: Entry[K, V], state: State): State = {
    val (oldVector, oldMap) = state
    val childIndices = getChildIndices(index, oldVector)
    if (heapPropertyViolated(entry.key, childIndices, oldVector)) {
      val minChildIndex = childIndices.minBy(oldVector(_).key)
      val stateAfterSwap = swap(index, minChildIndex, state)
      bubbleDown(minChildIndex, entry, stateAfterSwap)
    } else {
      state
    }
  }

  private def heapPropertyViolated(parentKey: K, childIndices: Seq[Int], vector: Vector[Entry[K, V]]): Boolean = {
    val childSmallerThanParent = childIndices.find {
      case index => keyOrdering.compare(parentKey, vector(index).key) > 0
    }
    childSmallerThanParent.isDefined
  }

  private def hasParent(index: Int) = index > 0
  private def getParentIndex(index: Int) = (Math.floor((index - 1) / 2)).toInt
  private def getChildIndices(index: Int, vector: Vector[_]) = 
    List(index * 2 + 1, index * 2 + 2).filter(_ < vector.size)

  private def swap(i: Int, j: Int, state: State): State = {
    val (oldVector, oldMap) = state
    val updatedVector = oldVector.updated(i, oldVector(j)).updated(j, oldVector(i))
    val updatedMap = oldMap + (oldVector(i).value -> j) + (oldVector(j).value -> i)
    (updatedVector, updatedMap)
  }

}

object VectorHeap {
  def apply[K : Ordering, V]() = new VectorHeap(Vector[Entry[K,V]](), Map[V, Int]())
}


type Id = Int
type Length = Int
case class Edge(tail: Id, head: Id, length: Length)
case class Vertex(id: Id, edgesOut: Seq[Edge])
type Graph = Map[Id, Vertex]

/** Initialize the heap of unprocessed vertices */
def buildUnprocessedHeap(graph: Graph, sourceVertex: Vertex): Heap[Length, Id] = {
  val unknownDistance = Int.MaxValue

  // Add source vertex's adjacent nodes to heap
  val heap = sourceVertex.edgesOut.foldLeft[Heap[Length, Id]](VectorHeap[Length, Id]()) {
    case (heap, e) => heap.insert(e.length, e.head)
  }

  // Add all other vertices to heap with unknown distance
  val unknown = graph.keys.filterNot{ i => 
       i == sourceVertex.id || sourceVertex.edgesOut.find(_.head == i).isDefined 
  }
  unknown.foldLeft[Heap[Length, Id]](heap) {
    case (heap, id) => heap.insert(unknownDistance, id)
  }
}

case class StepResult(processed: Set[Id], shortestPathLengths: Map[Id, Length], unprocessed: Heap[Length, Id])
/** Perform one step of Dijkstra's algorithm, i.e. add one vertex to the processed set. */
def dijkstraStep(graph: Graph,
                 processed: Set[Id], 
                 shortestPathLengths: Map[Id, Length], 
                 unprocessed: Heap[Length, Id]): StepResult = {
  val (nextHeapEntry, heapAfterExtractMin) = unprocessed.extractMin
  val shortestPath = nextHeapEntry.key
  val vertexId = nextHeapEntry.value

  // Update shortest path distances for adjacent vertices in V-X
  val updatedHeap = {
    graph(vertexId).edgesOut
                   .filterNot(processed contains _.head)
                   .foldLeft(heapAfterExtractMin) {
      case (heap, edge) => heap.decreaseKey(edge.head, shortestPath + edge.length)
    }
  }

  StepResult(processed + vertexId, shortestPathLengths + (vertexId -> shortestPath), updatedHeap)
}

/** 
 * Dijkstra's algorithm to compute shortest paths from the given source vertex to every other vertex
 */
def dijkstra(graph: Graph, sourceVertexId: Id): Map[Id, Length] = {
  var processed: Set[Id] = Set(sourceVertexId)
  var shortestPathLengths: Map[Id, Length] = Map(sourceVertexId -> 0)
  var unprocessed: Heap[Length, Id] = buildUnprocessedHeap(graph: Graph, graph(sourceVertexId))

  while (processed.size < graph.size) {
    val stepResult = dijkstraStep(graph, processed, shortestPathLengths, unprocessed)
    processed = stepResult.processed
    shortestPathLengths = stepResult.shortestPathLengths
    unprocessed = stepResult.unprocessed
  }

  shortestPathLengths
}

/* Input munging */
val input: Iterator[(Id, List[(Id, Length)])] = {
  io.Source.fromFile("dijkstraData.txt")
    .getLines
    .map(_.split("\t").toList)
    .collect { 
      case x :: xs => {
        val id = x.toInt
        val edges = xs.map(_.split(",").toList).collect {
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

