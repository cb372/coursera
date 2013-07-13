/*
 * Counting inversions in a list, piggybacking on merge-sort
 */

import collection.mutable._

// Normal mergesort, for reference
def merge(as: List[Int], bs: List[Int]): List[Int] = {
    val xs = ArrayBuffer[Int]()
    var (i, j) = (0, 0)
    while (i < as.size || j < bs.size) {
      if (i == as.size && j < bs.size) {
        xs += bs(j)
        j += 1
      } else if (i < as.size && j == bs.size) {
        xs += as(i)
        i += 1
      } else if (i < as.size && as(i) < bs(j)) {
        xs += as(i)
        i += 1
      } else if (j < bs.size) {
        xs += bs(j)
        j += 1
      }
    }
    xs.toList
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

def mergeAndCountSplitInv(as: List[Int], bs: List[Int]): (List[Int], Long) = {
    val xs = ArrayBuffer[Int]()
    var (i, j, inv: Long) = (0, 0, 0L)
    while (i < as.size || j < bs.size) {
      if (i == as.size && j < bs.size) {
        xs += bs(j)
        j += 1
      } else if (i < as.size && j == bs.size) {
        xs += as(i)
        i += 1
      } else if (i < as.size && as(i) < bs(j)) {
        xs += as(i)
        i += 1
      } else if (j < bs.size) {
        xs += bs(j)
        j += 1
        inv += (as.size - i)
      }
    }
    (xs.toList, inv)
}

def sortAndCountSplitInv(xs: List[Int]): (List[Int], Long) = xs match {
  case Nil => (Nil, 0)
  case xs @ (x :: Nil) => (xs, 0)
  case xs => {
    val half = xs.size / 2
    val (as, leftInv) = sortAndCountSplitInv(xs take half)
    val (bs, rightInv) = sortAndCountSplitInv(xs drop half)
    val (merged, splitInv) = mergeAndCountSplitInv(as, bs)
    (merged, leftInv + rightInv + splitInv)
  }
}

val input = io.Source.fromFile(new java.io.File("input.txt")).getLines.map(_.toInt).toList
