package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day6._

class Day6Test extends FunSuite {

  val exampleInput =
    """1, 1
      |1, 6
      |8, 3
      |3, 4
      |5, 5
      |8, 9""".stripMargin

  test("Part 1 examples") {
    assert(largestFiniteArea(parseCoords(exampleInput)) == 17)
  }

  test("Part 1 input answer") {
    assert(largestFiniteArea(parseCoords(input)) == 4771)
  }

  test("Part 2 examples") {
    assert(safeArea(parseCoords(exampleInput), 32) == 16)
  }

  test("Part 2 input answer") {
    assert(safeArea(parseCoords(input)) == 39149)
  }
}
