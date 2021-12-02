package eu.sim642.adventofcode2021

import Day2._
import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {

  val exampleInput =
    """forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2""".stripMargin

  test("Part 1 examples") {
    assert(Part1.multiplyFinalPos(parseCommands(exampleInput)) == 150)
  }

  test("Part 1 input answer") {
    assert(Part1.multiplyFinalPos(parseCommands(input)) == 1962940)
  }

  test("Part 2 examples") {
    assert(Part2.multiplyFinalPos(parseCommands(exampleInput)) == 900)
  }

  test("Part 2 input answer") {
    assert(Part2.multiplyFinalPos(parseCommands(input)) == 1813664422)
  }
}
