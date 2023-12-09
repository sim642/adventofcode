package eu.sim642.adventofcode2023

import Day9._
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {

  private val exampleInput =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45""".stripMargin

  test("Part 1 examples") {
    val histories = parseHistories(exampleInput)

    assert(nextValue(histories(0)) == 18)
    assert(nextValue(histories(1)) == 28)
    assert(nextValue(histories(2)) == 68)

    assert(sumNextValues(histories) == 114)
  }

  test("Part 1 input answer") {
    assert(sumNextValues(parseHistories(input)) == 1708206096)
  }
}
