package eu.sim642.adventofcode2022

import Day9._
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {

  val exampleInput =
    """R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2""".stripMargin

  val exampleInput2 =
    """R 5
      |U 8
      |L 8
      |D 3
      |R 17
      |D 10
      |L 25
      |U 20""".stripMargin

  test("Part 1 examples") {
    assert(countTailPoss(parseMoves(exampleInput), 2) == 13)
  }

  test("Part 1 input answer") {
    assert(countTailPoss(parseMoves(input), 2) == 5695)
  }

  test("Part 2 examples") {
    assert(countTailPoss(parseMoves(exampleInput), 10) == 1)
    assert(countTailPoss(parseMoves(exampleInput2), 10) == 36)
  }

  test("Part 2 input answer") {
    assert(countTailPoss(parseMoves(input), 10) == 2434)
  }
}
