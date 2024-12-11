package eu.sim642.adventofcode2024

import Day11._
import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite {

  val exampleInput = "0 1 10 99 999"
  val exampleInput2 = "125 17"

  test("Part 1 examples") {
    assert(countBlinkedStones(parseStones(exampleInput), 1) == 7)
    assert(countBlinkedStones(parseStones(exampleInput2), 6) == 22)
    assert(countBlinkedStones(parseStones(exampleInput2), 25) == 55312)
  }

  test("Part 1 input answer") {
    assert(countBlinkedStones(parseStones(input), 25) == 217812)
  }

  test("Part 2 input answer") {
    assert(countBlinkedStones(parseStones(input), 75) == 259112729857522L)
  }
}
