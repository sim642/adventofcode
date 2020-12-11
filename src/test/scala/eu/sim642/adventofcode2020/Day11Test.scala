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
    assert(Part1.countOccupiedStable(parseGrid(exampleInput)) == 37)
  }

  test("Part 1 input answer") {
    assert(Part1.countOccupiedStable(parseGrid(input)) == 2204)
  }

  test("Part 2 examples") {
    assert(Part2.countOccupiedStable(parseGrid(exampleInput)) == 26)
  }

  test("Part 2 input answer") {
    assert(Part2.countOccupiedStable(parseGrid(input)) == 1986)
  }
}
