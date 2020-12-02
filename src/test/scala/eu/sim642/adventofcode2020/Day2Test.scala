package eu.sim642.adventofcode2020

import Day2._
import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {

  val exampleInput =
    """1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc""".stripMargin

  test("Part 1 examples") {
    assert(Part1.countValid(parsePasswordPolicies(exampleInput)) == 2)
  }

  test("Part 1 input answer") {
    assert(Part1.countValid(parsePasswordPolicies(input)) == 383)
  }

  test("Part 2 examples") {
    assert(Part2.countValid(parsePasswordPolicies(exampleInput)) == 1)
  }

  test("Part 2 input answer") {
    assert(Part2.countValid(parsePasswordPolicies(input)) == 272)
  }
}
