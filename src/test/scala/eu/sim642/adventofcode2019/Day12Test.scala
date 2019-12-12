package eu.sim642.adventofcode2019

import org.scalatest.{FunSuite, Suites}
import Day12._
import eu.sim642.adventofcode2019.Day12Test._
import eu.sim642.adventofcodelib.pos.Pos3

class Day12Test extends Suites(
  new BaseTest,
  new NaivePart2SolutionTest,
  new LcmPart2SolutionTest,
)

object Day12Test {

  val exampleInput =
    """<x=-1, y=0, z=2>
      |<x=2, y=-10, z=-7>
      |<x=4, y=-8, z=8>
      |<x=3, y=5, z=-1>""".stripMargin

  val exampleInput2 =
    """<x=-8, y=-10, z=0>
      |<x=5, y=5, z=10>
      |<x=2, y=-7, z=3>
      |<x=9, y=-8, z=-3>""".stripMargin

  class BaseTest extends FunSuite {

    test("parseMoon") {
      assert(parseMoons(exampleInput) ==
        Seq(
          Moon(Pos3(-1, 0, 2), Pos3.zero),
          Moon(Pos3(2, -10, -7), Pos3.zero),
          Moon(Pos3(4, -8, 8), Pos3.zero),
          Moon(Pos3(3, 5, -1), Pos3.zero),
        )
      )
    }

    test("Part 1 examples") {
      assert(simulateTotalEnergy(parseMoons(exampleInput), 10) == 179)
      assert(simulateTotalEnergy(parseMoons(exampleInput2), 100) == 1940)
    }

    test("Part 1 input answer") {
      assert(simulateTotalEnergy(parseMoons(input)) == 13045)
    }

  }

  sealed abstract class Part2SolutionTest(part2Solution: Part2Solution) extends FunSuite {

    test("Part 2 example (short)") {
      assert(part2Solution.simulateCycleSteps(parseMoons(exampleInput)) == 2772)
    }

    protected val testLong: Boolean = true

    if (testLong) {
      test("Part 2 example (long)") {
        assert(part2Solution.simulateCycleSteps(parseMoons(exampleInput2)) == 4686774924L)
      }

      test("Part 2 input answer") {
        assert(part2Solution.simulateCycleSteps(parseMoons(input)) == 344724687853944L)
      }
    }
  }

  class NaivePart2SolutionTest extends Part2SolutionTest(NaivePart2Solution) {
    override protected val testLong: Boolean = false // takes forever
  }

  class LcmPart2SolutionTest extends Part2SolutionTest(LcmPart2Solution)
}
