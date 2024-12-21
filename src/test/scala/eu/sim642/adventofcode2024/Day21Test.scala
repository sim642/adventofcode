package eu.sim642.adventofcode2024

import Day21.*
import Day21Test.*
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends Suites(
  new NaiveSolutionTest,
  new DynamicProgrammingSolutionTest,
)

object Day21Test {

  val exampleInput =
    """029A
      |980A
      |179A
      |456A
      |379A""".stripMargin

  abstract class SolutionTest(solution: Solution) extends AnyFunSuite {
    import solution._

    test("Part 1 examples") {
      assert(shortestSequenceLength("029A", 0) == "<A^A>^^AvvvA".length)
      assert(shortestSequenceLength("029A", 1) == "v<<A>>^A<A>AvA<^AA>A<vAAA>^A".length)
      assert(shortestSequenceLength("029A", 2) == "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A".length)

      assert(sumCodeComplexity(parseCodes(exampleInput), part1DirectionalKeypads) == 126384)
    }

    test("Part 1 input answer") {
      assert(sumCodeComplexity(parseCodes(input), part1DirectionalKeypads) == 157892)
    }

    protected val testPart2: Boolean = true

    if (testPart2) {
      test("Part 2 examples") {
        assert(sumCodeComplexity(parseCodes(exampleInput), part2DirectionalKeypads) == 154115708116294L) // not in text
      }

      test("Part 2 input answer") {
        assert(sumCodeComplexity(parseCodes(input), part2DirectionalKeypads) == 197015606336332L)
      }
    }
  }

  class NaiveSolutionTest extends SolutionTest(NaiveSolution) {
    override protected val testPart2: Boolean = false
  }

  class DynamicProgrammingSolutionTest extends SolutionTest(DynamicProgrammingSolution)
}
