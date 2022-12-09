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

  test("Part 1 examples") {
    assert(countTailPoss(parseMoves(exampleInput)) == 13)
  }

  test("Part 1 input answer") {
    assert(countTailPoss(parseMoves(input)) == 5695)
  }
}
