package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.IteratorImplicits._

import scala.annotation.tailrec

object Day10 {

  def differencesProduct(jolts: Seq[Int]): Int = {
    val initialJolt = 0
    val builtinJolt = jolts.max + 3
    val allJolts = initialJolt +: builtinJolt +: jolts
    val diffs = allJolts.sorted.iterator.zipWithTail.map({ case (a, b) => b - a }).toSeq
    diffs.count(_ == 1) * diffs.count(_ == 3)
  }

  def countArrangements(jolts: Seq[Int]): Long = {
    val builtinJolt = jolts.max + 3
    val allJolts = builtinJolt +: jolts

    // a_0 = 1
    // a_1 = J(1) * a_0
    // a_2 = J(2) * a_0 + J(2) * a_1 = J(2) * (a_0 + a_1)
    // a_3 = J(3) * (a_0 + a_1 + a_2)
    // a_4 = J(4) * (a_1 + a_2 + a_3)
    // ...
    // a_n = J(n) * (a_(n-3) + a_(n-2) + a_(n-1))

    // TODO: try knot-tying instead?

    @tailrec
    def helper(jolts: List[Int], prevs: Map[Int, Long]): Long = jolts match {
      case Nil => prevs(builtinJolt)
      case jolt :: newJolts =>
        val joltValue = prevs(jolt - 3) + prevs(jolt - 2) + prevs(jolt - 1)
        val newPrevs = prevs + (jolt -> joltValue)
        helper(newJolts, newPrevs)
    }

    helper(allJolts.sorted.toList, Map(0 -> 1L).withDefaultValue(0))
  }


  def parseJolts(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(differencesProduct(parseJolts(input)))
    println(countArrangements(parseJolts(input)))
  }
}
