package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day1._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day1Test extends FunSuite with ScalaCheckPropertyChecks {

  test("Part 1 examples") {
    val massExpectedFuels = Table(
      ("mass", "expectedFuel"),
      (12, 2),
      (14, 2),
      (1969, 654),
      (100756, 33583),
    )

    forAll (massExpectedFuels) { (mass, expectedFuel) =>
      assert(Part1.requiredFuel(mass) == expectedFuel)
    }
  }

  test("Part 1 input answer") {
    assert(Part1.totalRequiredFuel(parseMasses(input)) == 3371958)
  }

  test("Part 2 examples") {
    val massExpectedFuels = Table(
      ("mass", "expectedFuel"),
      (12, 2),
      (1969, 966),
      (100756, 50346),
    )

    forAll (massExpectedFuels) { (mass, expectedFuel) =>
      assert(Part2.requiredFuel(mass) == expectedFuel)
    }
  }

  test("Part 2 input answer") {
    assert(Part2.totalRequiredFuel(parseMasses(input)) == 5055050)
  }
}
