package eu.sim642.adventofcode2021

import Day14._
import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite {

  val exampleInput =
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C""".stripMargin

  test("Part 1 examples") {
    val Input(polymer, rules) = parseInput(exampleInput)
    assert(applyRules(polymer, rules) == parsePolymer("NCNBCHB"))
    assert(applyRules(parsePolymer("NCNBCHB"), rules) == parsePolymer("NBCCNBBBCBHCB"))
    assert(elementCountDifference(parseInput(exampleInput)) == 1588)
  }

  test("Part 1 input answer") {
    assert(elementCountDifference(parseInput(input)) == 3009)
  }

  test("Part 2 examples") {
    assert(elementCountDifference(parseInput(exampleInput), 40) == 2188189693529L)
  }

  test("Part 2 input answer") {
    assert(elementCountDifference(parseInput(input), 40) == 3459822539451L)
  }
}
