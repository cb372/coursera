/*
 * Job scheduling using a greedy algorithm
 */

case class Job(weight: Int, length: Int)

def schedule(jobs: Seq[Job], lt: (Job, Job) => Boolean) = jobs.sortWith(lt)

/** Comparator that sorts by descending (weight - length) */
val weightMinusLength = (job1: Job, job2: Job) => {
  val diff1 = job1.weight - job1.length
  val diff2 = job2.weight - job2.length
  if (diff1 > diff2) true
  else if (diff1 == diff2) (job1.weight > job2.weight) // break ties
  else false
}

/** Comparator that sorts by descending (weight / length) */
val weightLengthRatio = (job1: Job, job2: Job) => {
  val ratio1 = job1.weight.toDouble / job1.length.toDouble
  val ratio2 = job2.weight.toDouble / job2.length.toDouble
  (ratio1 >= ratio2) // doesn't matter how we break ties
}

/** Calculate sum(w_j * C_j) */
def weightedSumOfCompletionTimes(jobs: Seq[Job]): Long = {
  val (weightedSum, _) = jobs.foldLeft ((0L, 0L)) { 
    case ((acc, prevCompletionTime), Job(w, l)) => {
     val nextCompletionTime = prevCompletionTime + l
     (acc + (w * nextCompletionTime), nextCompletionTime)
    }
  }
  weightedSum
}

val jobs: Seq[Job] = {
  io.Source.fromFile(new java.io.File("jobs.txt"))
    .getLines
    .toList
    .tail // skip first line (number of jobs)
    .map(_.split(" ").toList)
    .collect { case w :: l :: Nil => Job(w.toInt, l.toInt) }
}

val answer1 = weightedSumOfCompletionTimes(schedule(jobs, weightMinusLength))
val answer2 = weightedSumOfCompletionTimes(schedule(jobs, weightLengthRatio))
