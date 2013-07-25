/*
 * Kosaraju algorithm to find strongly connected components of a directed graph.
 */

case class Edge(tail: Int, head: Int) // a directed edge from tail to head
case class Vertex(id: Int, edges: Set[Edge]) {
  def addEdge(edge: Edge) = this.copy(edges = this.edges + edge)
}
case class Graph(vertices: Map[Int, Vertex])

/*
 * Build a graph from a list of directed edges
 */
def buildGraph(input: Iterator[Array[Int]]): Graph = {
  val vertices = input.foldLeft(Map[Int,Vertex]()) {
    case (vertices, line) => {
      val edge = Edge(line(0), line(1))

      // Add the edge to both the head and tail vertices,
      // so that we can DFS the graph in reverse
      val tail = vertices.getOrElse(edge.tail, Vertex(edge.tail, Set[Edge]()))
      val head = vertices.getOrElse(edge.head, Vertex(edge.head, Set[Edge]()))

      vertices + (tail.id -> tail.addEdge(edge)) + (head.id -> head.addEdge(edge))
    }
  }
  Graph(vertices)
}
  
val input: Iterator[Array[Int]] = {
  io.Source.fromFile(new java.io.File("SCC-10k.txt"))
            .getLines
            .map(_.split(" ").map(_.toInt))
}
val graph = buildGraph(input)


