package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day1 {

  def countIncreases(depths: Seq[Int], window: Int = 1): Int = {
    depths.sliding(window).map(_.sum).zipWithTail.count({ case (a, b) => a < b })
  }

  def parseDepths(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countIncreases(parseDepths(input)))
    println(countIncreases(parseDepths(input), 3))
  }
}
