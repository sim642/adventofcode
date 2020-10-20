package eu.sim642.adventofcode2018

import Day17._
import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {

  val exampleInput =
    """x=495, y=2..7
      |y=7, x=495..501
      |x=501, y=3..7
      |x=498, y=2..4
      |x=506, y=1..2
      |x=498, y=10..13
      |x=504, y=10..13
      |y=13, x=498..504""".stripMargin

  test("Part 1 examples") {
    assert(floodedTiles(parseInput(exampleInput)) == 57)
  }

  test("Settling water is not settled") {
    /*
    ...||+||..
    ...|###|..
    ...|...|..
    ||||/////#
    |#~~~~~~~#
    |#########
    */
    val settlingInput =
      """y=1, x=499..501
        |y=5, x=496..504
        |x=496, y=4..5
        |x=504, y=3..5""".stripMargin
    assert(floodedTiles(parseInput(settlingInput)) == 22)
  }

  test("Part 1 input answer") {
    assert(floodedTiles(parseInput(input)) == 29063)
  }

  test("Part 2 examples") {
    assert(settledTiles(parseInput(exampleInput)) == 29)
  }

  test("Part 2 input answer") {
    assert(settledTiles(parseInput(input)) == 23811)
  }
}
