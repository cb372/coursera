val HUGE: Float = 1000000F  // huge, but not big enough to cause overflow problems

type Node = (Double, Double)
val nodes: Array[Node] = {
  io.Source.fromFile(new java.io.File("tsp.txt"))
    .getLines
    .toList
    .tail
    .map(_.split(" ").toList)
    .collect { case a :: b :: Nil => (a.toDouble, b.toDouble) }
    .toArray
}
val n = nodes.size
 
def cost(i1: Int, i2: Int): Float = {
  import Math._
  val from = nodes(i1)
  val to = nodes(i2)
  sqrt(pow(from._1 - to._1, 2) + pow(from._2 - to._2, 2)).toFloat
}

def memoizeBinomCoeffs(max: Int) = {
  val result = collection.mutable.Map.empty[(Int, Int), Int]
  // base cases
  for (n <- 1 to max) {
    result((n, 0)) = 1
    result((n, n)) = 1
  }
  // recursion
  for {
    n <- 1 to max
    k <- 1 until n} {
    result((n, k)) = result((n-1, k-1)) + result((n-1, k))
  }
  result.toMap
}
val binomCoeffs: Map[(Int, Int), Int] = memoizeBinomCoeffs(n)
def choose(n: Int, k: Int): Int = binomCoeffs.getOrElse((n, k), 0)

def bitIsSet(bitset: Int, k: Int): Boolean = (bitset & (1 << k)) != 0

def index(combination: Int): Int = {
  var result = 0
  var k = 1
  for (i <- 0 until n) 
    if (bitIsSet(combination, i)) {
      val Ck = i
      result = result + choose(Ck, k)
      k = k + 1
    }
  result
}

def loopCombinations(n: Int, k: Int)(f: Int => Unit): Unit = {
  var set = (1 << k) - 1
  val limit = (1 << n)
  while (set < limit) {
    f(set)

    // Gosper's hack 
    // http://programmers.stackexchange.com/a/67087
    val c = set & -set
    val r = set + c
    set = (((r^set) >>> 2) / c) | r
  }
}

var prev = Array.ofDim[Float](choose(n, 1), n)
prev(index(1))(0) = 0F
for(i <- 1 until n) {
  val S = 1 << i
  prev(index(S))(0) = HUGE
}

def toNodeArray(S: Int): Array[Int] = {
  var result: List[Int] = Nil
  for (i <- 0 until n) 
    if (bitIsSet(S, i)) 
      result = i :: result
  result.toArray
}

for (m <- 2 to n) {
  println(s"m = $m, choose(n, m) = ${choose(n, m)}")
  val curr = Array.ofDim[Float](choose(n, m), n)
  loopCombinations(n, m){ S =>
    if (bitIsSet(S, 0)) {
      val nodes = toNodeArray(S)
      for (i <- 0 until nodes.size) {
        val j = nodes(i)
        if (j != 0) {
          val S_minus_j = S & ~(1 << j)
          var min = HUGE
          for (l <- (0 until nodes.size) if l != i) {
            val k = nodes(l)
            val value = prev(index(S_minus_j))(k) + cost(k, j)
            min = min.min(value)
          }
          curr(index(S))(j) = min
        }
      }
    }
  }
  prev = curr
}

var result = HUGE
val S = (1 to n).foldLeft(0){case (s, i) => s + (1 << i)}
for (j <- 2 to n) {
  println(s"${curr(index(S))(j)} + ${cost(j, 1)} = curr(index(S))(j) + cost(j, 1)")
  result = result.min(curr(index(S))(j) + cost(j, 1))
}
println(result)
