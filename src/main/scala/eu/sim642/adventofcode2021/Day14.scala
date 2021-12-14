package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.IterableImplicits._
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day14 {

  case class Polymer(elements: Map[Char, Int], pairs: Map[(Char, Char), Int])

  type Rules = Map[(Char, Char), Char]

  case class Input(polymer: Polymer, rules: Rules)

  def applyRules(polymer: Polymer, rules: Rules): Polymer = {
    val newPairs = polymer.pairs.iterator.flatMap({ case (pair@(a, b), cnt) =>
      val c = rules(pair)
      Iterator((a, c) -> cnt, (c, b) -> cnt)
    }).groupMapReduce(_._1)(_._2)(_ + _)
    val newElements = polymer.pairs.foldLeft(polymer.elements)({ case (elements, (pair, cnt)) =>
      val c = rules(pair)
      elements + (c -> (elements.getOrElse(c, 0) + cnt))
    })
    Polymer(newElements, newPairs)
  }

  def elementCountDifference(input: Input): Int = {
    val Input(initialPolymer, rules) = input
    val finalPolymer = Iterator.iterate(initialPolymer)(applyRules(_, rules))(10)
    val finalElementCounts = finalPolymer.elements.values
    finalElementCounts.max - finalElementCounts.min
  }


  def parsePolymer(s: String): Polymer = {
    val elements = s.iterator.groupCount(identity)
    val pairs = s.iterator.zipWithTail.groupCount(identity)
    Polymer(elements, pairs)
  }

  def parseRule(s: String): ((Char, Char), Char) = {
    val Seq(left, right) = s.split(" -> ", 2).toSeq
    (left(0), left(1)) -> right(0)
  }

  def parseInput(input: String): Input = {
    val Seq(polymerStr, rulesStr) = input.split("\n\n", 2).toSeq
    val polymer = parsePolymer(polymerStr)
    val rules = rulesStr.linesIterator.map(parseRule).toMap
    Input(polymer, rules)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(elementCountDifference(parseInput(input)))
  }
}
