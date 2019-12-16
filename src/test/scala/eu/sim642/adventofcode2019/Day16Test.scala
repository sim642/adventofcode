package eu.sim642.adventofcode2019

import Day16._
import eu.sim642.adventofcode2019.Day16Test._
import org.scalatest.{FunSuite, Suites}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day16Test extends Suites(
  new NaiveSolutionTest,
  new RangeSolutionTest,
  new UpperTriangularSolutionTest,
)

object Day16Test {

  sealed abstract class SolutionTest(solution: Solution) extends FunSuite with ScalaCheckPropertyChecks {

    test("Part 1 examples (small)") {
      val input = "12345678"

      val phasesExpectedEight = Table(
        ("phases", "expectedEight"),
        (1, "48226158"),
        (2, "34040438"),
        (3, "03415518"),
        (4, "01029498"),
      )

      forAll(phasesExpectedEight) { (phases, expectedEight) =>
        assert(solution.stepPhasesEight(parseSignal(input), phases) == expectedEight)
      }
    }

    test("Part 1 examples (large)") {
      val inputExpectedEight = Table(
        ("input", "expectedEight"),
        ("80871224585914546619083218645595", "24176176"),
        ("19617804207202209144916044189917", "73745418"),
        ("69317163492948606335995924319873", "52432133"),
      )

      forAll(inputExpectedEight) { (input, expectedEight) =>
        assert(solution.stepPhasesEight(parseSignal(input)) == expectedEight)
      }
    }

    test("Part 1 input answer") {
      assert(solution.stepPhasesEight(parseSignal(input)) == "25131128")
    }

    protected val testPart2: Boolean = true

    if (testPart2) {
      test("Part 2 examples") {
        val inputExpectedEight = Table(
          ("input", "expectedEight"),
          ("03036732577212944063491565474664", "84462026"),
          ("02935109699940807407585447034323", "78725270"),
          ("03081770884921959731165446850517", "53553731"),
        )

        forAll(inputExpectedEight) { (input, expectedEight) =>
          assert(solution.stepPhasesEightOffset(parseSignal(input)) == expectedEight)
        }
      }

      test("Part 2 input answer") {
        assert(solution.stepPhasesEightOffset(parseSignal(input)) == "53201602")
      }
    }
  }

  class NaiveSolutionTest extends SolutionTest(NaiveSolution) {
    override protected val testPart2: Boolean = false
  }

  class RangeSolutionTest extends SolutionTest(RangeSolution) {
    override protected val testPart2: Boolean = false
  }

  class UpperTriangularSolutionTest extends SolutionTest(UpperTriangularSolution)
}
