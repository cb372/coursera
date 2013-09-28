
type Node = Int
type Cost = Int
case class Edge(from: Node, to: Node, cost: Cost)
type Graph = Map[Node, List[Edge]]
type APSP = Array[Array[Option[Cost]]]

def hasNegativeCostCycle(A_k: APSP): Boolean = {
  (0 until A_k.size).map(i => A_k(i)(i)).flatten.find(_ < 0).isDefined
}

def floydWarshall(graph: Graph): Option[APSP] = {
  val n = graph.size
  var A_kMinus1: APSP = null
  var A_k: APSP = null

  println("Initialising...")
  // initialise
  A_k = Array.tabulate(n, n){ (i, j) =>
    if (i == j) Some(0)
    else graph(i+1).find(_.to == j+1).map(_.cost)
  }
  println()

  println("Triple-looping...")
  for (k <- 1 to n) {
    print(".")
    A_kMinus1 = A_k
    A_k = Array.ofDim[Option[Cost]](n, n)
    for {
      i <- 1 to n
      j <- 1 to n
    } {
      val case1: Option[Cost] = A_kMinus1(i-1)(j-1)
      val case2: Option[Cost] =
        for {
          a <- A_kMinus1(i-1)(k-1)
          b <- A_kMinus1(k-1)(j-1)
        } yield a + b
      A_k(i-1)(j-1) = List(case1, case2).flatten match {
        case Nil => None
        case xs => Some(xs.min)
      }
    }

    if (hasNegativeCostCycle(A_k)) return None
  }
  println()

  Some(A_k)
}

def minShortestPath(apsp: APSP): Cost = apsp.flatten.flatten.min

def loadEdges(filename: String): Iterator[Edge] = {
  io.Source.fromFile(filename)
    .getLines
    .drop(1)
    .map(_.split(" ").toList)
    .collect { case from :: to :: cost :: Nil => Edge(from.toInt, to.toInt, cost.toInt) }
}

def buildGraph(edges: Iterator[Edge]): Graph = {
  edges.foldLeft (Map[Node, List[Edge]]()) { (graph, edge) =>
    graph + (edge.from -> (edge :: graph.getOrElse(edge.from, Nil)))
  }
}

def calcMinShortestPath(filename: String): Option[Cost] = {
  val graph = buildGraph(loadEdges(filename))
  val apsp = floydWarshall(graph)
  val result = apsp.map(minShortestPath)
  println(s"Min shortest path for $filename = $result")
  result
}

val results = List(
  calcMinShortestPath("g1.txt"),
  calcMinShortestPath("g2.txt"),
  calcMinShortestPath("g3.txt")
).flatten

println(results match {
  case Nil => "NULL"
  case rs => rs.min
})
