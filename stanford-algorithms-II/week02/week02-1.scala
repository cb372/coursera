/*
 * k-clustering using a variant of Kruskal's MST algorithm
 */

import scala.annotation.tailrec
import com.github.cb372.collection.UnionFind

object Week02_1 extends App {
  type Id = Int
  type Cost = Int
  case class Edge(node1: Id, node2: Id, cost: Cost)

  def skipUnseparatedEdges(edges: Seq[Edge], unionFind: UnionFind[Id]): (Seq[Edge], UnionFind[Id]) = {
    var uf = unionFind
    val remainingEdges = edges.dropWhile{ e =>
      val (root1, uf1) = uf.find(e.node1)
      val (root2, uf2) = uf1.find(e.node2)
      uf = uf2
      (root1 == root2)
    }
    (remainingEdges, uf)
  }

  /**
   * Returns the maximum spacing of a k-cluster
   */
  def clusterSpacing(edges: Seq[Edge], k: Int): Cost = {
    val nodes = edges.foldLeft(Set[Id]()){(nodes, edge) => nodes + edge.node1 + edge.node2}
    var unionFind = UnionFind[Id](nodes.toSeq : _*)

    def recurse(edges: Seq[Edge], k: Int, unionFind: UnionFind[Id]): Cost = {
      if (unionFind.size <= k) {
        // We are down to k clusters -> finished.
        // Spacing of cluster is the cost of the next *separated* edge
        val (remainingEdges, _) = skipUnseparatedEdges(edges, unionFind)
        remainingEdges.head.cost 
      } else {
        val (remainingEdges, uf) = skipUnseparatedEdges(edges, unionFind)
        recurse(remainingEdges.tail, k, uf.union(remainingEdges.head.node1, remainingEdges.head.node2))
      }
    }

    recurse(edges, k, unionFind)
  }

  val edges: Seq[Edge] = {
    io.Source.fromFile("clustering1.txt")
      .getLines
      .toList
      .tail
      .map(_.split(" ").toList)
      .collect{ case n1 :: n2 :: c :: Nil => Edge(n1.toInt, n2.toInt, c.toInt) }
      .sortBy(_.cost)
  }

  println(clusterSpacing(edges, 4))

}
