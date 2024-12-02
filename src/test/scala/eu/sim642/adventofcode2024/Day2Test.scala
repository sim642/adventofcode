package eu.sim642.adventofcode2024

import Day2._
import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {

  val exampleInput =
    """7 6 4 2 1
      |1 2 7 8 9
      |9 7 6 2 1
      |1 3 2 4 5
      |8 6 4 4 1
      |1 3 6 7 9""".stripMargin

  test("Part 1 examples") {
    assert(countSafe(parseReports(exampleInput)) == 2)
  }

  test("Part 1 input answer") {
    assert(countSafe(parseReports(input)) == 549)
  }

  test("Part 2 examples") {
  }

  test("Part 2 input answer") {
  }
}
