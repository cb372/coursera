import com.github.cb372.collection.UnionFind

object Week02_2 extends App {

  type Node = Int
  type Cost = Int

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

  def clusters(nodes: Seq[Node], k: Int): Cost = {
    val unionFind = UnionFind(nodes: _*)

    // TODO
    0
  }

  val nodes: Seq[Node] = {
    io.Source.fromFile(new java.io.File("clustering_big.txt"))
      .getLines
      .drop(1)
      .map(_.split(" ").toList.map(_.toInt))
      .map(bitsToInt(_))
      .toSeq
  }

  println(nodes.take(10))
}
