package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day2 {

  type Report = Seq[Int]

  def isSafe(report: Report): Boolean = {
    val sorted = report.sorted
    val increasing = report == sorted
    val decreasing = report.reverse == sorted
    val monotonic = increasing || decreasing
    val safeDifferences = report.iterator.zipWithTail.forall({ case (a, b) =>
      val diff = (a - b).abs
      1 <= diff && diff <= 3
    })
    monotonic && safeDifferences
  }

  def countSafe(reports: Seq[Report]): Int = reports.count(isSafe)

  def parseReports(input: String): Seq[Report] = input.linesIterator.map(_.split(" ").map(_.toInt).toSeq).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countSafe(parseReports(input)))
  }
}
