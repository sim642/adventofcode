package eu.sim642.adventofcode2018

import org.scalatest.{FunSuite, Suites}
import Day23._
import eu.sim642.adventofcode2018.Day23Test.{CliquePart2SolutionTest, Part1Test, SplittingPart2SolutionTest}

class Day23Test extends Suites(
  new Part1Test,
  new CliquePart2SolutionTest,
  new SplittingPart2SolutionTest,
)

object Day23Test {

  class Part1Test extends FunSuite {

    val exampleInput1 =
      """pos=<0,0,0>, r=4
        |pos=<1,0,0>, r=1
        |pos=<4,0,0>, r=3
        |pos=<0,2,0>, r=1
        |pos=<0,5,0>, r=3
        |pos=<0,0,3>, r=1
        |pos=<1,1,1>, r=1
        |pos=<1,1,2>, r=1
        |pos=<1,3,1>, r=1""".stripMargin

    test("Part 1 examples") {
      assert(nanobotsInLargestRadius(parseInput(exampleInput1)) == 7)
    }

    test("Part 1 input answer") {
      assert(nanobotsInLargestRadius(parseInput(input)) == 219)
    }
  }

  sealed abstract class Part2SolutionTest(part2Solution: Part2Solution) extends FunSuite {

    val exampleInput2 =
      """pos=<10,12,12>, r=2
        |pos=<12,14,12>, r=2
        |pos=<16,12,12>, r=4
        |pos=<14,14,14>, r=6
        |pos=<50,50,50>, r=200
        |pos=<10,10,10>, r=5""".stripMargin

    test("Part 2 examples") {
      assert(part2Solution.closestMostNanobots(parseInput(exampleInput2)) == 36)
    }

    test("Part 2 input answer") {
      assert(part2Solution.closestMostNanobots(parseInput(input)) == 83779034)
    }
  }

  class CliquePart2SolutionTest extends Part2SolutionTest(CliquePart2Solution)

  class SplittingPart2SolutionTest extends Part2SolutionTest(SplittingPart2Solution)
}
