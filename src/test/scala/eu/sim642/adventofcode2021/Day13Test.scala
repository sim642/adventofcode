package eu.sim642.adventofcode2021

import Day13.*
import org.apache.commons.io.output.ByteArrayOutputStream
import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {

  val exampleInput =
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5""".stripMargin

  test("Part 1 examples") {
    assert(countDotsAfter1(parseInput(exampleInput)) == 17)
  }

  test("Part 1 input answer") {
    assert(countDotsAfter1(parseInput(input)) == 729)
  }

  test("Part 2 examples") {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      printFoldedDots(parseInput(exampleInput))
    }
    assert(out.toString.trim ==
      """#####
        |#...#
        |#...#
        |#...#
        |#####""".stripMargin)
  }

  test("Part 2 input answer") {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      printFoldedDots(parseInput(input)) // RGZLBHFP
    }
    assert(out.toString.trim ==
      """###...##..####.#....###..#..#.####.###.
        |#..#.#..#....#.#....#..#.#..#.#....#..#
        |#..#.#......#..#....###..####.###..#..#
        |###..#.##..#...#....#..#.#..#.#....###.
        |#.#..#..#.#....#....#..#.#..#.#....#...
        |#..#..###.####.####.###..#..#.#....#...""".stripMargin)
  }
}
