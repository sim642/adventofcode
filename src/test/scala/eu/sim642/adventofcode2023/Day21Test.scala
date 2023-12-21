package eu.sim642.adventofcode2023

import Day21.*
import Day21Test.*
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day21Test extends Suites(
  new Part1Test,
  new NaivePart2SolutionTest,
  new QuadraticPart2SolutionTest,
)

object Day21Test {

  private val exampleInput =
    """...........
      |.....###.#.
      |.###.##..#.
      |..#.#...#..
      |....#.#....
      |.##..S####.
      |.##..#...#.
      |.......##..
      |.##.#.####.
      |.##..##.##.
      |...........""".stripMargin

  class Part1Test extends AnyFunSuite {

    test("Part 1 examples") {
      assert(countReachableExactly(parseGrid(exampleInput), 6) == 16)
    }

    test("Part 1 input answer") {
      assert(countReachableExactly(parseGrid(input)) == 3687)
    }
  }

  class NaivePart2SolutionTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    test("Part 2 examples") {
      val stepsExpectedReachable = Table(
        ("steps", "expectedReachable"),
        (6, 16),
        (10, 50),
        (50, 1594),
        (100, 6536),
        // TODO: optimize
        //(500, 167004),
        //(1000, 668697),
        //(5000, 16733044),
      )

      forAll(stepsExpectedReachable) { (steps, expectedReachable) =>
        assert(NaivePart2Solution.countReachableExactlyInfinite(parseGrid(exampleInput), steps) == expectedReachable)
      }
    }
  }

  abstract class Part2SolutionTest(part2Solution: Part2Solution) extends AnyFunSuite with ScalaCheckPropertyChecks {

    test("Part 2 input answer") {
      // computed by NaivePart2Solution
      val stepsExpectedReachable = Table(
        ("steps", "expectedReachable"),
        // TODO: optimize
        //(64, 3687),
        //(128, 14452),
        //(256, 57375),
        //(512, 228690),
        //(1024, 912913),

        //(131, 15130),
        //(2 * 131, 60085),
        //(3 * 131, 134866),
        //(4 * 131, 239473),
        //(5 * 131, 373906),

        (65, 3778),
        (65 + 1 * 131, 33695),
        (65 + 2 * 131, 93438),
        (65 + 3 * 131, 183007),
        (65 + 4 * 131, 302402),
      )

      forAll(stepsExpectedReachable) { (steps, expectedReachable) =>
        assert(part2Solution.countReachableExactlyInfinite(parseGrid(input), steps) == expectedReachable)
      }

      assert(part2Solution.countReachableExactlyInfinite(parseGrid(input)) == 610321885082978L)
    }
  }

  class QuadraticPart2SolutionTest extends Part2SolutionTest(QuadraticPart2Solution)
}
