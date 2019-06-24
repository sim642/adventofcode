package eu.sim642.adventofcode2016

import Day10._
import org.scalatest.FunSuite

class Day10Test extends FunSuite {

  val exampleInput =
    """value 5 goes to bot 2
      |bot 2 gives low to bot 1 and high to bot 0
      |value 3 goes to bot 1
      |bot 1 gives low to output 1 and high to bot 0
      |bot 0 gives low to output 2 and high to output 0
      |value 2 goes to bot 2""".stripMargin

  test("Part 1 examples") {
    assert(findComparer(exampleInput, Set(5, 2)) == 2)
  }

  test("Part 1 input answer") {
    assert(findComparer(input) == 147)
  }
}
