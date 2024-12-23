package eu.sim642.adventofcode2024

import Day16.*
import Day16Test.*
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends Suites(
  new Part1Test,
  new BackwardNeighborsPart2SolutionTest,
  new AllPathsPart2SolutionTest,
)

object Day16Test {

  val exampleInput =
    """###############
      |#.......#....E#
      |#.#.###.#.###.#
      |#.....#.#...#.#
      |#.###.#####.#.#
      |#.#.#.......#.#
      |#.#.#####.###.#
      |#...........#.#
      |###.#.#####.#.#
      |#...#.....#.#.#
      |#.#.#.###.#.#.#
      |#.....#...#.#.#
      |#.###.#.#.#.#.#
      |#S..#.....#...#
      |###############""".stripMargin

  val exampleInput2 =
    """#################
      |#...#...#...#..E#
      |#.#.#.#.#.#.#.#.#
      |#.#.#.#...#...#.#
      |#.#.#.#.###.#.#.#
      |#...#.#.#.....#.#
      |#.#.#.#.#.#####.#
      |#.#...#.#.#.....#
      |#.#.#####.#.###.#
      |#.#.#.......#...#
      |#.#.###.#####.###
      |#.#.#...#.....#.#
      |#.#.#.#####.###.#
      |#.#.#.........#.#
      |#.#.#.#########.#
      |#S#.............#
      |#################""".stripMargin

  class Part1Test extends AnyFunSuite {
    test("Part 1 examples") {
      assert(lowestScore(parseGrid(exampleInput)) == 7036)
      assert(lowestScore(parseGrid(exampleInput2)) == 11048)
    }

    test("Part 1 input answer") {
      assert(lowestScore(parseGrid(input)) == 73404)
    }
  }

  class Part2SolutionTest(part2Solution: Part2Solution) extends AnyFunSuite {
    import part2Solution._

    test("Part 2 examples") {
      assert(bestPathTiles(parseGrid(exampleInput)) == 45)
      assert(bestPathTiles(parseGrid(exampleInput2)) == 64)
    }

    test("Part 2 input answer") {
      assert(bestPathTiles(parseGrid(input)) == 449)
    }
  }

  class BackwardNeighborsPart2SolutionTest extends Part2SolutionTest(BackwardNeighborsPart2Solution)

  class AllPathsPart2SolutionTest extends Part2SolutionTest(AllPathsPart2Solution)
}
