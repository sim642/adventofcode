package eu.sim642.adventofcode2023

import Day9.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day9Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  private val exampleInput =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45""".stripMargin

  test("Part 1 examples") {
    import Part1._
    
    val histories = parseHistories(exampleInput)

    val expectedExtrapolateds = Table(
      ("i", "expectedExtrapolate"),
      (0, 18),
      (1, 28),
      (2, 68),
    )

    forAll(expectedExtrapolateds) { (i, expectedExtrapolate) =>
      assert(extrapolate(histories(i)) == expectedExtrapolate)
    }

    assert(sumExtrapolated(histories) == 114)
  }

  test("Part 1 input answer") {
    assert(Part1.sumExtrapolated(parseHistories(input)) == 1708206096)
  }

  test("Part 2 examples") {
    import Part2._
    
    val histories = parseHistories(exampleInput)

    val expectedExtrapolateds = Table(
      ("i", "expectedExtrapolate"),
      (0, -3),
      (1, 0),
      (2, 5),
    )

    forAll(expectedExtrapolateds) { (i, expectedExtrapolate) =>
      assert(extrapolate(histories(i)) == expectedExtrapolate)
    }
  }

  test("Part 2 input answer") {
    assert(Part2.sumExtrapolated(parseHistories(input)) == 1050)
  }
}
