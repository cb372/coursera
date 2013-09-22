/*
 * Knapsack problem: recursive solution w/ memoisation
 *
 * Run with 
 * scala -J-Xmx2g -J-Xss500m week03-2.scala
 */
type Value = Long
type Weight = Int
case class Item(v: Value, w: Weight)

case class Problem(W: Weight, items: Array[Item]) {

  def solve: Value = {
    val A = collection.mutable.Map.empty[(Int, Weight), Value]

    def a(i: Int, x: Weight): Value = {
      if (A.contains((i, x))) {
        println(f"$i%04d\t$x%08d\tHit")
        A((i, x))
      }
      else if (i == 0) 
        0
      else {
        val left = a(i-1, x)
        val right = if (x >= items(i).w) a(i-1, x-items(i).w) + items(i).v else 0
        val max = left max right
        A((i, x)) = max // memoise result
        println(f"$i%04d\t$x%08d\tMemoise")
        max
      }
    }

    a(items.size - 1, W) // optimal value is top-right corner of array
  }

}

val problem = {
  val lines = io.Source.fromFile(new java.io.File("knapsack_big.txt"))
    .getLines
    .toList
    .map(_.split(" ").toList)
  val W = lines.head.head.toInt
  // Add dummy item to make items 1-indexed
  val items = Item(0, 0) :: lines.tail.collect { case v :: w :: Nil => Item(v.toLong, w.toInt) }
  Problem(W, items.toArray)
}

println(problem.solve)
