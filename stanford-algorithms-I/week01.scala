/*
 * Counting inversions in a list, piggybacking on merge-sort
 */

// Normal mergesort, for reference
def merge(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
  case (Nil, Nil) => Nil
  case (as, Nil) => as
  case (Nil, bs) => bs
  case (a::as, b::bs) if a < b => a :: (merge(as, b::bs))
  case (as, b::bs) => b :: merge(as, bs)
}

def mergesort(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case xs @ (x :: Nil) => xs
  case xs => {
    val half = xs.size / 2
    val as = mergesort(xs take half)
    val bs = mergesort(xs drop half)
    merge(as, bs)
  }
}

def mergeAndCountSplitInv(as: List[Int], bs: List[Int], acc: Int): (List[Int], Int) = (as, bs) match {
  case (Nil, Nil) => (Nil, acc)
  case (as, Nil) => (as, acc)
  case (Nil, bs) => (bs, acc)
  case (a::as, b::bs) if a < b => {
    val (merged, inv) = mergeAndCountSplitInv(as, b::bs, acc)
    (a :: merged, inv)
  }
  case (as, b::bs) => {
    val (merged, inv) = mergeAndCountSplitInv(as, bs, acc + as.size)
    (b :: merged, inv)
  }
}

def sortAndCountSplitInv(xs: List[Int]): (List[Int], Int) = xs match {
  case Nil => (Nil, 0)
  case xs @ (x :: Nil) => (xs, 0)
  case xs => {
    val half = xs.size / 2
    val (as, leftInv) = sortAndCountSplitInv(xs take half)
    val (bs, rightInv) = sortAndCountSplitInv(xs drop half)
    mergeAndCountSplitInv(as, bs, leftInv + rightInv)
  }
}

val input = io.Source.fromFile(new java.io.File("input.txt")).getLines.map(_.toInt).toList
