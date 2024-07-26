package eu.sim642.adventofcode2019

import org.scalatest.Suites
import Day22._
import eu.sim642.adventofcode2019.Day22Test._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class Day22Test extends Suites(
  new NaivePart1SolutionTest,
  new PositionPart1SolutionTest,
  new LinearSolutionTest,
)

object Day22Test {

  val exampleInput =
    """deal with increment 7
      |deal into new stack
      |deal into new stack""".stripMargin

  val exampleInput2 =
    """cut 6
      |deal with increment 7
      |deal into new stack""".stripMargin

  val exampleInput3 =
    """deal with increment 7
      |deal with increment 9
      |cut -2""".stripMargin

  val exampleInput4 =
    """deal into new stack
      |cut -2
      |deal with increment 7
      |cut 8
      |cut -4
      |deal with increment 7
      |cut 3
      |deal with increment 9
      |deal with increment 3
      |cut -1""".stripMargin


  trait Part1SolutionTest extends AnyFunSuite {
    val solution: Part1Solution

    test("Part 1 input answer") {
      assert(solution.shuffleFactoryOrderPosition(parseTechniques(input)) == 7665)
    }
  }

  trait Part2SolutionTest extends AnyFunSuite {
    val solution: Part2Solution

    test("Part 2 input answer") {
      assert(solution.shuffleFactoryOrderPositionInverse(parseTechniques(input)) == 41653717360577L)
    }
  }


  class NaivePart1SolutionTest extends Part1SolutionTest with ScalaCheckPropertyChecks {
    override val solution: Part1Solution = NaivePart1Solution

    test("Part 1 examples") {
      val inputExpectedDeck = Table(
        ("input", "expectedDeck"),
        ("deal into new stack", Seq(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)),
        ("cut 3", Seq(3, 4, 5, 6, 7, 8, 9, 0, 1, 2)),
        ("deal with increment 3", Seq(0, 7, 4, 1, 8, 5, 2, 9, 6, 3)),
        (exampleInput, Seq(0, 3, 6, 9, 2, 5, 8, 1, 4, 7)),
        (exampleInput2, Seq(3, 0, 7, 4, 1, 8, 5, 2, 9, 6)),
        (exampleInput3, Seq(6, 3, 0, 7, 4, 1, 8, 5, 2, 9)),
        (exampleInput4, Seq(9, 2, 5, 8, 1, 4, 7, 0, 3, 6)),
      )

      forAll(inputExpectedDeck) { (input, expectedDeck) =>
        assert(NaivePart1Solution.shuffleFactoryOrder(parseTechniques(input), 10) == expectedDeck)
      }
    }
  }

  class PositionPart1SolutionTest extends Part1SolutionTest {
    override val solution: Part1Solution = PositionPart1Solution
  }

  class LinearSolutionTest extends Part1SolutionTest with Part2SolutionTest {
    override val solution: Part1Solution & Part2Solution = LinearSolution
  }
}
