package eu.sim642.adventofcode2023

import Day9._
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {

  private val exampleInput =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45""".stripMargin

  test("Part 1 examples") {
    import Part1._
    
    val histories = parseHistories(exampleInput)

    assert(extrapolate(histories(0)) == 18)
    assert(extrapolate(histories(1)) == 28)
    assert(extrapolate(histories(2)) == 68)

    assert(sumExtrapolated(histories) == 114)
  }

  test("Part 1 input answer") {
    assert(Part1.sumExtrapolated(parseHistories(input)) == 1708206096)
  }

  test("Part 2 examples") {
    import Part2._
    
    val histories = parseHistories(exampleInput)

    assert(extrapolate(histories(0)) == -3)
    assert(extrapolate(histories(1)) == 0)
    assert(extrapolate(histories(2)) == 5)
  }

  test("Part 2 input answer") {
    assert(Part2.sumExtrapolated(parseHistories(input)) == 1050)
  }
}
