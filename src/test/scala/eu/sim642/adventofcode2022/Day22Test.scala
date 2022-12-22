package eu.sim642.adventofcode2022

import Day22._
import org.scalatest.funsuite.AnyFunSuite

class Day22Test extends AnyFunSuite {

  val exampleInput =
    """        ...#
      |        .#..
      |        #...
      |        ....
      |...#.......#
      |........#...
      |..#....#....
      |..........#.
      |        ...#....
      |        .....#..
      |        .#......
      |        ......#.
      |
      |10R5L5R10L4R5L5""".stripMargin

  test("Part 1 examples") {
    assert(Part1.finalPassword(parseInput(exampleInput)) == 6032)
  }

  test("Part 1 input answer") {
    assert(Part1.finalPassword(parseInput(input)) == 89224)
  }

  test("Part 2 examples") {
    assert(Part2.finalPassword(parseInput(exampleInput), ExampleCubeNet) == 5031)
  }

  test("Part 2 input answer") {
    assert(Part2.finalPassword(parseInput(input)) == 136182)
  }
}
