package eu.sim642.adventofcode2021

import Day20._
import org.scalatest.funsuite.AnyFunSuite

class Day20Test extends AnyFunSuite {

  val exampleInput =
    """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
      |
      |#..#.
      |#....
      |##..#
      |..#..
      |..###""".stripMargin

  test("Part 1 examples") {
    assert(countEnhanced(parseInput(exampleInput)) == 35)
  }

  test("Part 1 input answer") {
    assert(countEnhanced(parseInput(input)) == 5680)
  }

  test("Part 2 examples") {
    assert(countEnhanced(parseInput(exampleInput), 50) == 3351)
  }

  test("Part 2 input answer") {
    assert(countEnhanced(parseInput(input), 50) == 19766)
  }
}
