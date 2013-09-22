/*
 * Knapsack problem: naive solution
 */
type Value = Long
type Weight = Int
case class Item(v: Value, w: Weight)

case class Problem(W: Weight, items: List[Item]) {

  def solve: Value = {
    val A = collection.mutable.Map.empty[(Int, Weight), Value]

    // initialise
    for (x <- 0 to W) A((0, x)) = 0

    // populate array
    for {
      x <- 0 to W
      (item, i) <- items.zipWithIndex.tail // skip the dummy item
    } {
      val left = A((i-1, x))
      val right = if (x >= item.w) A((i-1, x-item.w)) + item.v else 0
      A((i, x)) = left max right
    }
    
    A((items.size - 1), W) // optimal value is top-right corner of array
  }

}

val problem = {
  val lines = io.Source.fromFile(new java.io.File("knapsack1.txt"))
    .getLines
    .toList
    .map(_.split(" ").toList)
  val W = lines.head.head.toInt
  // Add dummy item to make items 1-indexed
  val items = Item(0, 0) :: lines.tail.collect { case v :: w :: Nil => Item(v.toLong, w.toInt) }
  Problem(W, items)
}
