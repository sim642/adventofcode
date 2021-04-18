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

  test("Part 2 generalizations") {
    // https://old.reddit.com/r/adventofcode/comments/kc94h1/2020_day_13_part_2_generalization/

    val busesExpectedDepart = Table(
      ("buses", "expectedDepart"),
      ("14,x,x,x,335,x,x,x,39,x,x,x,x,x,x,x,x,187,19", 124016326L),
      ("73,x,x,x,x,x,x,67,x,25,x,x,x,x,x,343,x,x,9", 369373941L),
      ("7,24,x,x,9,13,x,x,x,20,x,x,x,33", 173831L),
      ("71,x,x,x,x,x,x,x,375,x,x,x,x,x,x,x,x,726,x,x,x,x,x,76,67,53,x,x,x,94", 21428909746117L),
      ("173,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,1287,x,x,2173,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,1275,x,x,x,x,x,x,x,x,x,x,x,671,x,x,x,x,x,x,2674", 27208285429450535L),
      ("1997,x,x,x,x,x,x,1747,x,x,x,x,x,2003,x,x,x,x,x,x,1883,x,x,x,x,x,1667,x,x,x,x,x,x,x,1701", 4756544012204563475L),
    )

    val busesExpectedContradiction = Table(
      "buses",
      "77,97,x,x,x,x,x,x,57,x,x,x,x,x,62,x,x,x,x,78,x,x,x,65",
      "59,x,x,x,117,x,x,x,x,x,x,x,x,x,x,x,189,x,61,x,x,137",
    )

    forAll(busesExpectedDepart) { (buses, expectedDepart) =>
      assert(earliestSubsequentDepart(parseBuses(buses)) == BigInt(expectedDepart))
    }

    forAll(busesExpectedContradiction) { (buses) =>
      assertThrows[NoSuchElementException](earliestSubsequentDepart(parseBuses(buses)))
    }
  }

  test("Part 2 input answer") {
    assert(earliestSubsequentDepart(parseNotes(input)) == 905694340256752L)
  }
}
