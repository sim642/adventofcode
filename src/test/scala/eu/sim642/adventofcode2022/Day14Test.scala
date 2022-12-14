package eu.sim642.adventofcode2022

import Day14._
import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite {

  val exampleInput =
    """498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

  test("Part 1 examples") {
    assert(countRestingSand(parseTiles(exampleInput)) == 24)
  }

  test("Part 1 input answer") {
    assert(countRestingSand(parseTiles(input)) == 665)
  }
}
