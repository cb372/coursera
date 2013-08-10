/*
 * Median maintenance using 2 heaps.
 */
import collection.mutable.PriorityQueue
import math.Ordering

val xs = {
  io.Source.fromFile("Median.txt")
    .getLines
    .map(_.toInt)
}

def calcSumOfMedians(xs: Iterator[Int]): Int = {
  var sumOfMedians = 0
  val lowerHalf = PriorityQueue.empty[Int] // max-heap
  val upperHalf = PriorityQueue.empty[Int](implicitly[Ordering[Int]].reverse) // min-heap

  def insert(x: Int): Unit = {
    if (lowerHalf.isEmpty || x < lowerHalf.head)
      lowerHalf += x
    else
      upperHalf += x
  }

  def diff = lowerHalf.size - upperHalf.size

  def rebalanceHeaps(): Unit = {
    while (diff < -1) {
      // Upper half is too big. Move one elem to lower half
      lowerHalf += upperHalf.dequeue()
    }
    while (diff > 1) {
      // Lower half is too big. Move one elem to upper half
      upperHalf += lowerHalf.dequeue()
    }
    assert (diff == -1 || diff == 0 || diff == 1)
  }

  def median: Int = diff match {
    case -1 => upperHalf.head // k is odd, upper half is bigger
    case 0 => lowerHalf.head // k is even
    case 1 => lowerHalf.head // k is odd, lower half is bigger
    case _ => sys.error("Heaps are unbalanced!")
  }

  for (x <- xs) {
    insert(x)
    rebalanceHeaps()
    sumOfMedians += median
  }

  sumOfMedians
}

println(calcSumOfMedians(xs) % 10000)
