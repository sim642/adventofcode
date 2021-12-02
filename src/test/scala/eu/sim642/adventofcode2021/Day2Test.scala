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
    assert(multiplyFinalPos(parseCommands(exampleInput)) == 150)
  }

  test("Part 1 input answer") {
    assert(multiplyFinalPos(parseCommands(input)) == 1962940)
  }
}
