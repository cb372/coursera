/* Quicksort, counting comparisons along the way */

def exampleArray = Array(3, 8, 2, 5, 1, 4, 7, 6)

/* Swap two elements in the given array */
def swap(xs: Array[Int], a: Int, b: Int): Unit = {
  val tmp = xs(a)
  xs(a) = xs(b)
  xs(b) = tmp
}

/*
 * Partitions the given subarray. Assumes first element is pivot.
 * Returns the final index of the pivot element.
 */
def partition(xs: Array[Int], left: Int, right: Int): Int = {
  val p = xs(left)
  var i = left + 1
  for (j <- (left + 1) until right) {
    if (xs(j) < p) {
      swap(xs, i, j)
      i += 1
    }
  }
  val newPIndex = i - 1
  // move pivot to correct position
  swap(xs, left, newPIndex)
  newPIndex
}

/* The length of a subarray */
def len(left: Int, right: Int) = right - left

/* 
 * Comparisons performed during partitioning = N - 1
 * because pivot is compared with each of the other elements.
 */
def countComparisons(left: Int, right: Int): Int = len(left, right) - 1

def qsort(xs: Array[Int], left: Int, right: Int, choosePivotIndex: (Array[Int], Int, Int) => Int): Int = {
  if (left >= right - 1) 
    // Base case: subarray of length 1
    return 0

  // Choose pivot
  val pIndex = choosePivotIndex(xs, left, right)

  // Move the pivot to the start
  swap(xs, pIndex, left)

  // Partition around the pivot
  val newPIndex = partition(xs, left, right)

  // Count the comparisons performed
  val comparisons = countComparisons(left, right)

  // Recurse
  val leftComparisons = qsort(xs, left, newPIndex, choosePivotIndex)
  val rightComparisons = qsort(xs, (newPIndex + 1), right, choosePivotIndex)

  // Return total comparison count
  (comparisons + leftComparisons + rightComparisons)
}

/*
 * Sorts the input array in-place and returns the
 * total number of comparisons performed.
 * Takes the pivot strategy as an argument.
 */
def quicksort(xs: Array[Int], choosePivotIndex: (Array[Int], Int, Int) => Int): Int = 
  qsort(xs, 0, xs.length, choosePivotIndex)

/*
 * Pivot strategies
 */

def firstElement(xs: Array[Int], left: Int, right: Int): Int = left
def lastElement(xs: Array[Int], left: Int, right: Int): Int = right - 1
def medianOfThree(xs: Array[Int], left: Int, right: Int): Int = {
  val middle = 
    if (len(left, right) % 2 == 0)
      left + (len(left, right) / 2) - 1
    else
      left + (len(left, right) / 2)

  val (leftValue, middleValue, rightValue) = (xs(left), xs(middle), xs(right - 1))
  val sortedByValue = 
    List((left, leftValue), (middle, middleValue), ((right - 1), rightValue)).sortBy(_._2)
  sortedByValue(1)._1
}

/*
 * I/O
 */
val input = io.Source.fromFile(new java.io.File("week02-input.txt")).getLines.map(_.toInt).toList
