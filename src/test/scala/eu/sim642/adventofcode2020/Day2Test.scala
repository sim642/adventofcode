package eu.sim642.adventofcode2020

import Day2._
import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {

  val exampleInput =
    """1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc""".stripMargin

  test("Part 1 examples") {
    assert(countValid(parsePasswordPolicies(exampleInput)) == 2)
  }

  test("Part 1 input answer") {
    assert(countValid(parsePasswordPolicies(input)) == 383)
  }
}
