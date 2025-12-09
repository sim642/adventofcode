package eu.sim642.adventofcode2025

import Day9._
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {

  val exampleInput =
    """7,1
      |11,1
      |11,7
      |9,7
      |9,5
      |2,5
      |2,3
      |7,3""".stripMargin

  test("Part 1 examples") {
    assert(Part1.largestArea(parseRedTiles(exampleInput)) == 50)
  }

  test("Part 1 input answer") {
    assert(Part1.largestArea(parseRedTiles(input)) == 4729332959L)
  }

  test("Part 2 examples") {
    assert(Part2.largestArea(parseRedTiles(exampleInput)) == 24)
  }

  ignore("Part 2 input answer") { // TODO: optimize (~8.5s)
    assert(Part2.largestArea(parseRedTiles(input)) == 1474477524L)
  }
}
