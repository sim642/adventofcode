package eu.sim642.adventofcode2023

import Day24._
import org.scalatest.funsuite.AnyFunSuite

class Day24Test extends AnyFunSuite {

  private val exampleInput =
    """19, 13, 30 @ -2,  1, -2
      |18, 19, 22 @ -1, -1, -2
      |20, 25, 34 @ -2, -2, -4
      |12, 31, 28 @ -1, -2, -1
      |20, 19, 15 @  1, -5, -3""".stripMargin

  test("Part 1 examples") {
    assert(countIntersections1(parseHailstones(exampleInput), 7, 27) == 2)
  }

  test("Part 1 input answer") {
    assert(countIntersections1(parseHailstones(input)) == 21785)
  }
}
