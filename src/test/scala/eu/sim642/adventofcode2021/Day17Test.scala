package eu.sim642.adventofcode2021

import Day17._
import Day17Test._
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends Suites(
  new Part1Test,
  new SimulatePart2SolutionTest,
  new AxisTimePart2SolutionTest,
)

object Day17Test {

  val exampleInput = "target area: x=20..30, y=-10..-5"

  class Part1Test extends AnyFunSuite {

    test("Part 1 examples") {
      val exampleTarget = parseTarget(exampleInput)

      assert(hitsTargetY(exampleTarget, 2))
      assert(hitsTargetY(exampleTarget, 3))
      assert(hitsTargetY(exampleTarget, 0))
      //assert(!hitsTargetY(exampleTarget, -4)) // x velocity matters for not hitting in example
      assert(hitsTargetY(exampleTarget, 9))

      assert(findHighestY(exampleTarget) == 45)
    }

    test("Part 1 input answer") {
      assert(findHighestY(parseTarget(input)) == 2775)
    }
  }

  sealed abstract class Part2SolutionTest(part2Solution: Part2Solution) extends AnyFunSuite {

    test("Part 2 examples") {
      val expectedHitsTarget = Set(
        Pos(23, -10), Pos(25, -9), Pos(27, -5), Pos(29, -6), Pos(22, -6), Pos(21, -7), Pos(9, 0), Pos(27, -7), Pos(24, -5),
        Pos(25, -7), Pos(26, -6), Pos(25, -5), Pos(6, 8), Pos(11, -2), Pos(20, -5), Pos(29, -10), Pos(6, 3), Pos(28, -7),
        Pos(8, 0), Pos(30, -6), Pos(29, -8), Pos(20, -10), Pos(6, 7), Pos(6, 4), Pos(6, 1), Pos(14, -4), Pos(21, -6),
        Pos(26, -10), Pos(7, -1), Pos(7, 7), Pos(8, -1), Pos(21, -9), Pos(6, 2), Pos(20, -7), Pos(30, -10), Pos(14, -3),
        Pos(20, -8), Pos(13, -2), Pos(7, 3), Pos(28, -8), Pos(29, -9), Pos(15, -3), Pos(22, -5), Pos(26, -8), Pos(25, -8),
        Pos(25, -6), Pos(15, -4), Pos(9, -2), Pos(15, -2), Pos(12, -2), Pos(28, -9), Pos(12, -3), Pos(24, -6), Pos(23, -7),
        Pos(25, -10), Pos(7, 8), Pos(11, -3), Pos(26, -7), Pos(7, 1), Pos(23, -9), Pos(6, 0), Pos(22, -10), Pos(27, -6),
        Pos(8, 1), Pos(22, -8), Pos(13, -4), Pos(7, 6), Pos(28, -6), Pos(11, -4), Pos(12, -4), Pos(26, -9), Pos(7, 4),
        Pos(24, -10), Pos(23, -8), Pos(30, -8), Pos(7, 0), Pos(9, -1), Pos(10, -1), Pos(26, -5), Pos(22, -9), Pos(6, 5),
        Pos(7, 5), Pos(23, -6), Pos(28, -10), Pos(10, -2), Pos(11, -1), Pos(20, -9), Pos(14, -2), Pos(29, -7), Pos(13, -3),
        Pos(23, -5), Pos(24, -8), Pos(27, -9), Pos(30, -7), Pos(28, -5), Pos(21, -10), Pos(7, 9), Pos(6, 6), Pos(21, -5),
        Pos(27, -10), Pos(7, 2), Pos(30, -9), Pos(21, -8), Pos(22, -7), Pos(24, -9), Pos(20, -6), Pos(6, 9), Pos(29, -5),
        Pos(8, -2), Pos(27, -8), Pos(30, -5), Pos(24, -7)
      )

      assert(part2Solution.iterateHitsTarget(parseTarget(exampleInput)).iterator.toSet == expectedHitsTarget)

      assert(part2Solution.countHitsTarget(parseTarget(exampleInput)) == 112)
    }

    test("Part 2 input answer") {
      assert(part2Solution.countHitsTarget(parseTarget(input)) == 1566)
    }
  }

  class SimulatePart2SolutionTest extends Part2SolutionTest(SimulatePart2Solution)

  class AxisTimePart2SolutionTest extends Part2SolutionTest(AxisTimePart2Solution)
}
