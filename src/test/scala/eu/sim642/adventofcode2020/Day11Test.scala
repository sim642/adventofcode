package eu.sim642.adventofcode2020

import Day11._
import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite {

  val exampleInput =
    """L.LL.LL.LL
      |LLLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLLL
      |L.LLLLLL.L
      |L.LLLLL.LL""".stripMargin

  test("Part 1 examples") {
    assert(countOccupiedStable(parseGrid(exampleInput)) == 37)
  }

  test("Part 1 input answer") {
    assert(countOccupiedStable(parseGrid(input)) == 2204)
  }
}
