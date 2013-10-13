/*
 * Decide satisfiability of 2-SAT problems in linear time
 * by reduction to finding strongly connected components of a directed graph.
 * 
 * See http://www.math.ucsd.edu/~sbuss/CourseWeb/Math268_2007WS/2SAT.pdf
 */

import SCC._
import java.io.File

object Week6 extends App {
  def loadFile(input: File): List[Array[Int]] = {
    /* Load the clauses and convert every clause (u OR v) to 2 edges:
     *  1. ~u -> v
     *  2. ~v -> u
     */
    val lines = io.Source.fromFile(input).getLines.toList
    val edges = lines.tail
      .map(_.split(" "))
      .foldLeft(List[Array[Int]]()){ case (es, line) => 
        val u = line(0).toInt
        val v = line(1).toInt
        Array(-1 * u, v) :: Array(-1 * v, u) :: es 
      }
    (n, edges)
  }

  def isSatisfiable(input: File): Boolean = {
    val (n, edges) = loadFile(input)
    val graph = buildGraph(edges.iterator)
    val sccs = findSCCs(graph)
    /*
     * Constraint is satisfiable iff there is no SCC containing
     * both some value x_i and its negation ~(x_i)
     */
    val containsBothAValueAndItsNegation = sccs.map { scc: Set[Id] =>
      scc.find(i => scc.contains(-1 * i)).isDefined
    }
    !(containsBothAValueAndItsNegation.find(_ == true).isDefined)
  }

  for (i <- 1 to 6) {
    val filename = s"2sat$i.txt"
    val result = isSatisfiable(new File(filename))
    println(s"$filename is satisfiable: $result")
  }
}
