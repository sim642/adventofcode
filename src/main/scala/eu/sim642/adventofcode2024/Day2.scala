package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.SeqImplicits._

object Day2 {

  type Report = Seq[Int]

  trait Part {
    def isSafe(report: Report): Boolean

    def countSafe(reports: Seq[Report]): Int = reports.count(isSafe)
  }

  object Part1 extends Part {
    override def isSafe(report: Report): Boolean = {
      val sorted = report.sorted
      val increasing = report.isSorted
      val decreasing = report.isSorted(using Ordering[Int].reverse)
      val monotonic = increasing || decreasing
      val safeDifferences = report.iterator.zipWithTail.forall({ case (a, b) =>
        (1 to 3).contains((a - b).abs)
      })
      monotonic && safeDifferences
    }
  }

  object Part2 extends Part {
    override def isSafe(report: Report): Boolean = {
      Part1.isSafe(report) || report.indices.view.map(report.removed).exists(Part1.isSafe)
    }
  }

  def parseReports(input: String): Seq[Report] = input.linesIterator.map(_.split(" ").map(_.toInt).toSeq).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countSafe(parseReports(input)))
    println(Part2.countSafe(parseReports(input)))
  }
}
