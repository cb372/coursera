/*
 * Prim's algorithm for finding Minimum Spanning Tree of an undirected graph
 */

import annotation.tailrec

type Id = Int
case class Edge(u: Id, v: Id, cost: Int)
case class Node(id: Id, edges: Set[Edge])
case class Graph(nodes: Map[Id, Node], edges: Set[Edge])

/** Returns an MST of the given graph */
def prim(graph: Graph): Set[Edge] = {

  /** Find the cheapest edge with one end in X and the other end not in X */
  def findCheapestCrossingEdge(edges: Set[Edge], X: Map[Id, Node]): Edge = {
    edges
      .filter(e => 
        ((X contains e.u) && !(X contains e.v)) ||
        ((X contains e.v) && !(X contains e.u))
      ) 
      .toList
      .sortBy(_.cost)
      .head
  }

  @tailrec
  def primRecursive(graph: Graph, X: Map[Id, Node], T: Set[Edge]): Set[Edge] = {
    if (X.size == graph.nodes.size) // finished: we have a spanning tree
      T
    else {
      val e = findCheapestCrossingEdge(graph.edges, X)
      val v = if (X contains e.u) graph.nodes(e.v) else graph.nodes(e.u)
      primRecursive(graph, X + (v.id -> v), T + e)
    }
  }

  // Initialise X (nodes processed so far) = arbitrarily chosen node, T (tree-so-far) = empty set
  val X = Map(graph.nodes.head)
  val T = Set[Edge]()

  primRecursive(graph, X, T)
}

def buildGraph(edges: Seq[Edge]): Graph = {
  val nodes = edges.foldLeft (Map[Id, Node]()) { (nodes, edge) =>
    val uNode = nodes.getOrElse(edge.u, Node(edge.u, Set[Edge]()))
    val vNode = nodes.getOrElse(edge.v, Node(edge.v, Set[Edge]()))

    ((nodes + (edge.u -> uNode.copy(edges = uNode.edges + edge)))
            + (edge.v -> vNode.copy(edges = vNode.edges + edge)))
  }
  Graph(nodes, edges.toSet)
}

val edges: Seq[Edge] = {
  io.Source.fromFile(new java.io.File("edges.txt"))
    .getLines
    .toList
    .tail // skip header
    .map(_.split(" ").toList)
    .collect { case u :: v :: cost :: Nil => Edge(u.toInt, v.toInt, cost.toInt) }
}
val graph = buildGraph(edges)
val mst = prim(graph)

// WEIRD: sum gives completely the wrong answer?!
val wrongTotalCost = mst.map(_.cost.toLong).sum // gives -3444810
val totalCost = mst.foldLeft(0L){ (acc, edge) => acc + edge.cost } // gives correct answer: -3612829
