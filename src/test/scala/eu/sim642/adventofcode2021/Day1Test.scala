package eu.sim642.adventofcode2021

import Day1._
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {

  val exampleInput =
    """199
      |200
      |208
      |210
      |200
      |207
      |240
      |269
      |260
      |263""".stripMargin

  test("Part 1 examples") {
    assert(countIncreases(parseDepths(exampleInput)) == 7)
  }

  test("Part 1 input answer") {
    assert(countIncreases(parseDepths(input)) == 1301)
  }

  test("Part 2 examples") {
    assert(countIncreases(parseDepths(exampleInput), 3) == 5)
  }

  test("Part 2 input answer") {
    assert(countIncreases(parseDepths(input), 3) == 1346)
  }
}
