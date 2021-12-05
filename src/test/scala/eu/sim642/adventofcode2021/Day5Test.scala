package eu.sim642.adventofcode2021

import Day5._
import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {

  val exampleInput =
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2""".stripMargin

  test("Part 1 examples") {
    assert(countOverlaps(parseLines(exampleInput)) == 5)
  }

  test("Part 1 input answer") {
    assert(countOverlaps(parseLines(input)) == 5197)
  }
}
