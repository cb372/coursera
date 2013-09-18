import com.github.cb372.collection.UnionFind

import annotation.tailrec

object Week02_2 extends App {
  type Node = Int

  def bitsToInt(bits: List[Int]): Int = {
    bits.reverse.zipWithIndex.foldLeft(0) {  
      case (acc, (b, i)) => acc + (b << i)
    }
  }

  /**
   * Find all possible values with a Hamming distance <= 2 from the given value
   */
  def nearbyValues(value: Int): Seq[Int] = {
    val hamm1s = for {
      i <- 0 to 23
      hamm1 = value ^ (1 << i)
    } yield hamm1

    val hamm2s = for {
      i <- 0 to 23
      hamm1 = value ^ (1 << i)
      j <- 0 to 23 if j != i
      hamm2 = hamm1 ^ (1 << j)
    } yield hamm2

    hamm1s ++ hamm2s
  }

  /**
   * Count how many clusters are needed to ensure
   * that no pairs of nodes with Hamming distance <= 2
   * get split into separate clusters
   */
  def clusters(nodes: Set[Node]): Int = {
    val nodesList = nodes.toList
    val unionFind = UnionFind(nodesList: _*)

    @tailrec
    def recurse(remainingNodes: List[Node], unionFind: UnionFind[Node]): Int = remainingNodes match {
      case Nil => unionFind.size // finished -> return number of clusters
      case n::ns => {
        // Put the current node and any nodes near it into the same cluster
        val nearbyNodes = nearbyValues(n).filter(nodes.contains(_))
        val updatedUnionFind = nearbyNodes.foldLeft(unionFind){ (uf, nearbyNode) => uf.union(n, nearbyNode) }
        if (ns.size % 1000 == 0) println(s"${ns.size} nodes to go!")
        recurse(ns, updatedUnionFind)
      }
    }

    recurse(nodesList, unionFind)
  }

  val nodes: Set[Node] = {
    io.Source.fromFile(new java.io.File("clustering_big.txt"))
      .getLines
      .drop(1)
      .map(_.split(" ").toList.map(_.toInt))
      .map(bitsToInt(_))
      .toSet
  }

  println(clusters(nodes))
}
