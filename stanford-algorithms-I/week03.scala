/*
 * Karger algorithm for finding minimum cut of an undirected graph
 */
import scala.util.Random

case class Vertex(id: Int, adjacent: Vector[Int])
case class Graph(vertices: Map[Int, Vertex], nextVertexId: Int)
case class Edge(u: Int, v: Int)

/*
 * Build a graph from an adjacency list representation
 */
def buildGraph(input: List[List[Int]]): Graph = {
  val (vertices, maxId) = input.foldLeft((Map[Int,Vertex](), 0)) {
    case ((vertices, maxIdSoFar), line) => {
      val vertex = Vertex(line.head, line.tail.toVector)
      (vertices + (vertex.id -> vertex), maxIdSoFar max vertex.id)
    }
  }
  Graph(vertices, maxId + 1)
}
  
def chooseRandomEdge(graph: Graph): Edge = {
  def chooseRandomVertex(graph: Graph): Vertex = {
    val keys = graph.vertices.keys.toList
    val key = keys(Random.nextInt(keys.size))
    graph.vertices(key)
  }
  def chooseRandomEdgeFrom(vertex: Vertex): Edge = {
    val toVertexId = vertex.adjacent(Random.nextInt(vertex.adjacent.size))
    Edge(vertex.id, toVertexId)
  }
  chooseRandomEdgeFrom(chooseRandomVertex(graph))
}

/**
 * Merge vertices u and v (each side of the given edge) into a new vertex w.
 */
def mergeVertices(edge: Edge, graph: Graph): Vertex = {
  val u = graph.vertices(edge.u)
  val v = graph.vertices(edge.v)

  // All edges from u or v now go from w.
  // Any adges from u to v or vice versa now go from w to w, so can be deleted. 
  // This includes the edge that we originally randomly chose to remove.
  val updatedAdjacent = (u.adjacent.filterNot(_ == v.id) ++ v.adjacent.filterNot(_ == u.id))
  Vertex(graph.nextVertexId, updatedAdjacent)
}

/**
 * Redirect all edges that go to u or v, so that they point to w.
 */
def redirectEdges(uv: Edge, w: Vertex, vertices: Map[Int, Vertex]): Map[Int, Vertex] = {
  vertices.mapValues { vertex => 
    vertex.copy(
      adjacent = vertex.adjacent.map {
        case uv.u => w.id
        case uv.v => w.id
        case a => a
      }
    )
  }
}

/**
 * Remove a random edge from the graph, merging the edge's endpoints u and v
 * into a new vertex w.
 */
def contract(graph: Graph): Graph = {
  // Choose a random edge (u,v) to remove
  val uv = chooseRandomEdge(graph)

  // Merge u and v into a single vertex w.
  // All edges from u or v now go from w
  val w = mergeVertices(uv, graph)
  val replacedVertices = graph.vertices - uv.u - uv.v + (w.id -> w)

  // Update graph's edges so that all edges that went to u or v now go to w
  val withUpdatedEdges = redirectEdges(uv, w, replacedVertices)

  // Return the new graph
  Graph(withUpdatedEdges, w.id + 1)
}

def countEdgesFrom(vertex: Vertex): Int = vertex.adjacent.size

def minCut(graph: Graph): Int = {
  var contractedGraph = graph
  while (contractedGraph.vertices.size > 2) {
    contractedGraph = contract(contractedGraph)
  }
  // Return the number of edges between the 2 remaining vertices
  countEdgesFrom(contractedGraph.vertices.values.head)
}

def repeatedMinCuts(graph: Graph, trials: Int): Int = {
  var result = Int.MaxValue
  for (i <- 1 to trials) {
    val trialResult = minCut(graph)
    result = result min trialResult
    println(s"Result: $trialResult.\tBest result after trial $i: $result")
  }
  result
}

val input: List[List[Int]] = io.Source.fromFile(new java.io.File("kargerMinCut.txt")).getLines.map(_.split("\t").map(_.toInt).toList).toList
val graph = buildGraph(input)

// Run n^2 ln n trials
val n = graph.vertices.size
val trials = n * n * math.log(n).toInt
val answer = repeatedMinCuts(graph, trials)
println(s"Answer after $trials trials: $answer")

