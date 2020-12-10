package eu.sim642.adventofcode2020

import Day10._
import Day10Test._
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends Suites(
  new Part1Test,
  new DynamicProgrammingPart2SolutionTest,
  new ListKnotTyingPart2SolutionTest,
  new MapKnotTyingPart2SolutionTest,
)

object Day10Test {

  val exampleInput =
    """16
      |10
      |15
      |5
      |1
      |11
      |7
      |19
      |6
      |12
      |4""".stripMargin

  val exampleInput2 =
    """28
      |33
      |18
      |42
      |31
      |14
      |46
      |20
      |48
      |47
      |24
      |23
      |49
      |45
      |19
      |38
      |39
      |11
      |1
      |32
      |25
      |35
      |8
      |17
      |7
      |9
      |4
      |2
      |34
      |10
      |3""".stripMargin

  class Part1Test extends AnyFunSuite {

    test("Part 1 examples") {
      assert(differencesProduct(parseJolts(exampleInput)) == 7 * 5)
      assert(differencesProduct(parseJolts(exampleInput2)) == 22 * 10)
    }

    test("Part 1 input answer") {
      assert(differencesProduct(parseJolts(input)) == 2070)
    }
  }

  sealed abstract class Part2SolutionTest(part2Solution: Part2Solution) extends AnyFunSuite {

    test("Part 2 examples") {
      assert(part2Solution.countArrangements(parseJolts(exampleInput)) == 8)
      assert(part2Solution.countArrangements(parseJolts(exampleInput2)) == 19208)
    }

    test("Part 2 input answer") {
      assert(part2Solution.countArrangements(parseJolts(input)) == 24179327893504L)
    }
  }

  class DynamicProgrammingPart2SolutionTest extends Part2SolutionTest(DynamicProgrammingPart2Solution)

  class ListKnotTyingPart2SolutionTest extends Part2SolutionTest(ListKnotTyingPart2Solution)

  class MapKnotTyingPart2SolutionTest extends Part2SolutionTest(MapKnotTyingPart2Solution)
}
