package eu.sim642.adventofcode2024

import Day1._
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {

  val exampleInput =
    """3   4
      |4   3
      |2   5
      |1   3
      |3   9
      |3   3""".stripMargin

  test("Part 1 examples") {
    assert(totalListDistance(parseLists(exampleInput)) == 11)
  }

  test("Part 1 input answer") {
    assert(totalListDistance(parseLists(input)) == 1666427)
  }
}
