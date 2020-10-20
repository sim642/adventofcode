package eu.sim642.adventofcode2017

import Day13._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  val exampleRanges = Map(0 -> 3, 1 -> 2, 4 -> 4, 6 -> 4)

  test("Part 1 example positions") {
    val timePositions = Table(
      ("time", "positions"),
      (0, Map(0 -> 0, 1 -> 0, 4 -> 0, 6 -> 0)),
      (1, Map(0 -> 1, 1 -> 1, 4 -> 1, 6 -> 1)),
      (2, Map(0 -> 2, 1 -> 0, 4 -> 2, 6 -> 2)),
      (3, Map(0 -> 1, 1 -> 1, 4 -> 3, 6 -> 3))
    )

    forAll (timePositions) { (time, positions) =>
      assert(rangesPositions(exampleRanges, time) == positions)
    }
  }

  test("Part 1 example caught") {
    assert(rangesCaught(exampleRanges) == Set(0, 6))
  }

  test("Part 1 example severity") {
    assert(tripSeverity(exampleRanges) == 24)
  }

  test("Part 1 input answer") {
    assert(tripSeverity(input) == 1900)
  }

  test("Part 2 example caught") {
    assert(rangesCaught(exampleRanges, 10) == Set.empty)
  }

  test("Part 2 example uncaught delay") {
    assert(uncaughtDelay(exampleRanges) == 10)
  }

  test("Part 2 input answer") {
    assert(uncaughtDelay(input) == 3966414)
  }
}
