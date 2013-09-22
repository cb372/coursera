/*
 * Optimal binary search tree (question 5)
 */

val p = List(0, 0.05,0.4,0.08,0.04,0.1,0.1,0.23)
val A = collection.mutable.Map.empty[(Int,Int),Double]


for {
  s <- 0 to 6
  i <- 1 to 7
} {
  A((i, i + s)) = {
    val c: Double = p.slice(i, i+s+1).sum
    if (s==0) println(s"s=$s i=$i c=$c")
    for {
      r <- i to (i+s)
      first = if (i <= r-1) A.getOrElse((i, r-1), 0.0) else 0.
      second = if (r+1 <= i+s) A.getOrElse((r+1, i+s), 0.0) else 0.0
    } yield c + first + second
  }.min
}

for (j <- (1 to 7).reverse) {
  for (i <- 1 to 7) print(A.getOrElse((i, j), 0.0) + "\t")
  println()
}
