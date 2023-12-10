package eu.sim642.adventofcode2023

import Day10.*
import Day10Test.*
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends Suites(
  new Part1Test,
  new RayCastingPart2SolutionTest,
  new PicksTheoremPart2SolutionTest,
)

object Day10Test {

  private val exampleInput =
    """.....
      |.S-7.
      |.|.|.
      |.L-J.
      |.....""".stripMargin

  private val exampleInput2 =
    """-L|F7
      |7S-7|
      |L|7||
      |-L-J|
      |L|-JF""".stripMargin

  private val exampleInput3 =
    """..F7.
      |.FJ|.
      |SJ.L7
      ||F--J
      |LJ...""".stripMargin

  private val exampleInput4 =
    """7-F7-
      |.FJ|7
      |SJLL7
      ||F--J
      |LJ.LJ""".stripMargin

  private val exampleInput5 =
    """...........
      |.S-------7.
      |.|F-----7|.
      |.||.....||.
      |.||.....||.
      |.|L-7.F-J|.
      |.|..|.|..|.
      |.L--J.L--J.
      |...........""".stripMargin

  private val exampleInput6 =
    """..........
      |.S------7.
      |.|F----7|.
      |.||....||.
      |.||....||.
      |.|L-7F-J|.
      |.|..||..|.
      |.L--JL--J.
      |..........""".stripMargin

  private val exampleInput7 =
    """.F----7F7F7F7F-7....
      |.|F--7||||||||FJ....
      |.||.FJ||||||||L7....
      |FJL7L7LJLJ||LJ.L-7..
      |L--J.L7...LJS7F-7L7.
      |....F-J..F7FJ|L7L7L7
      |....L7.F7||L7|.L7L7|
      |.....|FJLJ|FJ|F7|.LJ
      |....FJL-7.||.||||...
      |....L---J.LJ.LJLJ...""".stripMargin

  private val exampleInput8 =
    """FF7FSF7F7F7F7F7F---7
      |L|LJ||||||||||||F--J
      |FL-7LJLJ||||||LJL-77
      |F--JF--7||LJLJ7F7FJ-
      |L---JF-JLJ.||-FJLJJ7
      ||F|F-JF---7F7-L7L|7|
      ||FFJF7L7F-JF7|JL---7
      |7-L-JL7||F7|L7F-7F7|
      |L.L7LFJ|||||FJL7||LJ
      |L7JLJL-JLJLJL--JLJ.L""".stripMargin

  class Part1Test extends AnyFunSuite {

    test("Part 1 examples") {
      assert(farthestDistance(parseGrid(exampleInput)) == 4)
      assert(farthestDistance(parseGrid(exampleInput2)) == 4)
      assert(farthestDistance(parseGrid(exampleInput3)) == 8)
      assert(farthestDistance(parseGrid(exampleInput4)) == 8)
    }

    test("Part 1 input answer") {
      assert(farthestDistance(parseGrid(input)) == 7063)
    }
  }

  abstract class Part2SolutionTest(part2Solution: Part2Solution) extends AnyFunSuite {

    test("Part 2 examples") {
      assert(part2Solution.enclosedTiles(parseGrid(exampleInput5)) == 4)
      assert(part2Solution.enclosedTiles(parseGrid(exampleInput6)) == 4)
      assert(part2Solution.enclosedTiles(parseGrid(exampleInput7)) == 8)
      assert(part2Solution.enclosedTiles(parseGrid(exampleInput8)) == 10)
    }

    test("Part 2 input answer") {
      assert(part2Solution.enclosedTiles(parseGrid(input)) == 589)
    }
  }

  class RayCastingPart2SolutionTest extends Part2SolutionTest(RayCastingPart2Solution)

  class PicksTheoremPart2SolutionTest extends Part2SolutionTest(PicksTheoremPart2Solution)
}
