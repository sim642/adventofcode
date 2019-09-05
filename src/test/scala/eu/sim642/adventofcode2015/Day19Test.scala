package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day19._

class Day19Test extends FunSuite {

  val exampleInputReplacements =
    """H => HO
      |H => OH
      |O => HH""".stripMargin

  val exampleInputReplacements2 =
    """e => H
      |e => O
      |H => HO
      |H => OH
      |O => HH""".stripMargin

  test("Part 1 examples") {
    val exampleReplacements = parseReplacements(exampleInputReplacements)
    assert(countDistinctSingleReplacements(exampleReplacements, "HOH") == 4)
    assert(countDistinctSingleReplacements(exampleReplacements, "HOHOHO") == 7)
  }

  test("Part 1 input answer") {
    assert(countDistinctSingleReplacements(input) == 509)
  }

  test("Part 2 examples") {
    val exampleReplacements2 = parseReplacements(exampleInputReplacements2)
    assert(fewestStepsFabricate(exampleReplacements2, "HOH") == 3)
    assert(fewestStepsFabricate(exampleReplacements2, "HOHOHO") == 6)
  }
}
