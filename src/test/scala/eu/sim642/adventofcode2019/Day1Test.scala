package eu.sim642.adventofcode2019

import org.scalatest.Suites
import Day1._
import eu.sim642.adventofcode2019.Day1Test._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends Suites(
  new Part1Test,
  new RecursivePart2SolutionTest,
  new ClosedFormPart2SolutionTest,
)

object Day1Test {

  class Part1Test extends AnyFunSuite with ScalaCheckPropertyChecks {

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
  }

  sealed abstract class Part2SolutionTest(part2Solution: Part2Solution) extends AnyFunSuite with ScalaCheckPropertyChecks {

    test("Part 2 examples") {
      val massExpectedFuels = Table(
        ("mass", "expectedFuel"),
        (12, 2),
        (1969, 966),
        (100756, 50346),
      )

      forAll (massExpectedFuels) { (mass, expectedFuel) =>
        assert(part2Solution.requiredFuel(mass) == expectedFuel)
      }
    }

    test("Part 2 input answer") {
      assert(part2Solution.totalRequiredFuel(parseMasses(input)) == 5055050)
    }
  }

  class RecursivePart2SolutionTest extends Part2SolutionTest(RecursivePart2Solution)

  class ClosedFormPart2SolutionTest extends Part2SolutionTest(ClosedFormPart2Solution) {

    test("Equivalent to recursive") {
      forAll ("mass") { mass: Int =>
        whenever (mass >= 0) {
          assert(ClosedFormPart2Solution.requiredFuel(mass) == RecursivePart2Solution.requiredFuel(mass))
        }
      }
    }
  }
}
