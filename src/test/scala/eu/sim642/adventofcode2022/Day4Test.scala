package eu.sim642.adventofcode2022

import Day4._
import org.scalatest.funsuite.AnyFunSuite

class Day4Test extends AnyFunSuite {

  val exampleInput =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8""".stripMargin

  test("Part 1 examples") {
    assert(countFullyContained(parsePairs(exampleInput)) == 2)
  }

  test("Part 1 input answer") {
    assert(countFullyContained(parsePairs(input)) == 441)
  }

  test("Part 2 examples") {
    assert(countOverlapping(parsePairs(exampleInput)) == 4)
  }

  test("Part 2 input answer") {
    assert(countOverlapping(parsePairs(input)) == 861)
  }
}
