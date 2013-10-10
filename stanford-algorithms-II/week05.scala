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
 
/** Euclidean distance between the points at the given indices */
def cost(i1: Int, i2: Int): Float = {
  import Math._
  val from = nodes(i1)
  val to = nodes(i2)
  sqrt(pow(from._1 - to._1, 2) + pow(from._2 - to._2, 2)).toFloat
}

/** Recursively calculate and memoize all values of aCb
 *  for 1 <= a, b <= n
 */
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

/** n C k */
def choose(n: Int, k: Int): Int = binomCoeffs.getOrElse((n, k), 0)

/** Check if the given bit is set in the given bitset */
def bitIsSet(bitset: Int, k: Int): Boolean = (bitset & (1 << k)) != 0

/** Decide the array index of the given bitset using combinatorial numbering:
 *  N = choose(c_k, k) + ... + choose(c_1, 1)
 */
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

/** Loop through all choose(n, k) bitsets */
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

/** Turn a bitset into an array of node IDs for debugging */
def toNodeArray(S: Int): Array[Int] = {
  var result: List[Int] = Nil
  for (i <- 0 until n) 
    if (bitIsSet(S, i)) 
      result = i :: result
  result.toArray
}

/* 2-D array built during previous iteration of the m-loop */
var prev: Array[Array[Float]] = null

/* 2-D array that we are currently building */
var curr: Array[Array[Float]] = null

/** Lookup a value in the 2-D array, 
 *  with special treatment for the base cases
 */
def lookup(i: Int, k: Int, m: Int): Float = {
  //println(s"lookup($i, $k, $m)")
  if (m == 2) {
    if (i== 0 && k == 0) return 0F // base case 1
    else return HUGE
  } 
  else if (k == 0) return HUGE // base case 2
  else return prev(i)(k - 1)
}

for (m <- 2 to n) {
  println(s"m = $m, choose(n, m) = ${choose(n, m)}")
  curr = Array.fill[Float](choose(n, m), n - 1)(HUGE)
  loopCombinations(n, m){ S =>
    if (bitIsSet(S, 0)) {
      for(j <- (1 until n) if bitIsSet(S, j)) { // for j in S, j != 1
        val S_minus_j = S & ~(1 << j)
        var min = HUGE
        // A[S, j] = min over k in S, k !=j { A[S-{j}, k] + C_kj }
        for (k <- (0 until n) if k != j && bitIsSet(S, k)) { 
          val value = lookup(index(S_minus_j), k, m) + cost(k, j)
          min = min.min(value)
        }
        curr(index(S))(j - 1) = min
      }
      //println(s"A[${toNodeArray(S).mkString("(", ",", ")")} = ${index(S)}] = ${curr(index(S)).mkString("\t")}")
    }
  }
  prev = curr
}

var result = HUGE
for (j <- 1 until n) {
  //println(s"curr(0)(j-1) + cost(j, 0) = ${curr(0)(j-1)} + ${cost(j, 0)}")
  result = result.min(curr(0)(j-1) + cost(j, 0))
}
println(result.toInt)
