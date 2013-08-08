/*
 * Variant of 2-SUM problem:
 * How many t in [-10000,10000] such that
 * there exist distinct integers (x, y) in the list such that x + y = t
 */

val input = {
  io.Source.fromFile("2sum.txt")
           .getLines
           .map(_.toLong)
           .toList
}

val set = input.toSet

def hasDistinct2sum(t: Long): Boolean = {
  input.find { x =>
    val y = t - x
    (y != x && (set contains y))
  }.isDefined
}

def findNumbersWithDistinct2sums = (-10000L to 10000L).par.filter(hasDistinct2sum)

println(findNumbersWithDistinct2sums.size)
