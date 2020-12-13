package eu.sim642.adventofcode2020

import Day13._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day13Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  val exampleInput =
    """939
      |7,13,x,x,59,x,31,19""".stripMargin

  test("Part 1 examples") {
    assert(earliestBusWaitTime(parseNotes(exampleInput)) == 295)
  }

  test("Part 1 input answer") {
    assert(earliestBusWaitTime(parseNotes(input)) == 203)
  }

  test("Part 2 examples") {
    val busesExpectedDepart = Table(
      ("buses", "expectedDepart"),
      ("7,13,x,x,59,x,31,19", 1068781),
      ("17,x,13,19", 3417),
      ("67,7,59,61", 754018),
      ("67,x,7,59,61", 779210),
      ("67,7,x,59,61", 1261476),
      ("1789,37,47,1889", 1202161486),
    )

    forAll(busesExpectedDepart) { (buses, expectedDepart) =>
      assert(earliestSubsequentDepart(parseBuses(buses)) == expectedDepart)
    }
  }

  test("Part 2 input answer") {
    assert(earliestSubsequentDepart(parseNotes(input)) == 905694340256752L)
  }
}
