package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day2 {

  type Report = Seq[Int]

  trait Part {
    def isSafe(report: Report): Boolean

    def countSafe(reports: Seq[Report]): Int = reports.count(isSafe)
  }

  object Part1 extends Part {
    override def isSafe(report: Report): Boolean = {
      val sorted = report.sorted
      val increasing = report == sorted // TODO: add isSorted to library
      val decreasing = report.reverse == sorted // TODO: add isSorted to library (with Ordering)
      val monotonic = increasing || decreasing
      val safeDifferences = report.iterator.zipWithTail.forall({ case (a, b) =>
        (1 to 3).contains((a - b).abs)
      })
      monotonic && safeDifferences
    }
  }

  object Part2 extends Part {
    override def isSafe(report: Report): Boolean = {
      Part1.isSafe(report) || report.indices.exists(i => Part1.isSafe(report.slice(0, i) ++ report.slice(i + 1, report.size))) // TODO: better seq removal? optimize?
    }
  }

  def parseReports(input: String): Seq[Report] = input.linesIterator.map(_.split(" ").map(_.toInt).toSeq).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countSafe(parseReports(input)))
    println(Part2.countSafe(parseReports(input)))
  }
}
