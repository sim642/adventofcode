package eu.sim642.adventofcode2022

import Day10.*
import org.apache.commons.io.output.ByteArrayOutputStream
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {

  val exampleInput =
    """noop
      |addx 3
      |addx -5""".stripMargin

  val exampleInput2 =
    """addx 15
      |addx -11
      |addx 6
      |addx -3
      |addx 5
      |addx -1
      |addx -8
      |addx 13
      |addx 4
      |noop
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx -35
      |addx 1
      |addx 24
      |addx -19
      |addx 1
      |addx 16
      |addx -11
      |noop
      |noop
      |addx 21
      |addx -15
      |noop
      |noop
      |addx -3
      |addx 9
      |addx 1
      |addx -3
      |addx 8
      |addx 1
      |addx 5
      |noop
      |noop
      |noop
      |noop
      |noop
      |addx -36
      |noop
      |addx 1
      |addx 7
      |noop
      |noop
      |noop
      |addx 2
      |addx 6
      |noop
      |noop
      |noop
      |noop
      |noop
      |addx 1
      |noop
      |noop
      |addx 7
      |addx 1
      |noop
      |addx -13
      |addx 13
      |addx 7
      |noop
      |addx 1
      |addx -33
      |noop
      |noop
      |noop
      |addx 2
      |noop
      |noop
      |noop
      |addx 8
      |noop
      |addx -1
      |addx 2
      |addx 1
      |noop
      |addx 17
      |addx -9
      |addx 1
      |addx 1
      |addx -3
      |addx 11
      |noop
      |noop
      |addx 1
      |noop
      |addx 1
      |noop
      |noop
      |addx -13
      |addx -19
      |addx 1
      |addx 3
      |addx 26
      |addx -30
      |addx 12
      |addx -1
      |addx 3
      |addx 1
      |noop
      |noop
      |noop
      |addx -9
      |addx 18
      |addx 1
      |addx 2
      |noop
      |noop
      |addx 9
      |noop
      |noop
      |noop
      |addx -1
      |addx 2
      |addx -37
      |addx 1
      |addx 3
      |noop
      |addx 15
      |addx -21
      |addx 22
      |addx -6
      |addx 1
      |noop
      |addx 2
      |addx 1
      |noop
      |addx -10
      |noop
      |noop
      |addx 20
      |addx 1
      |addx 2
      |addx 2
      |addx -6
      |addx -11
      |noop
      |noop
      |noop""".stripMargin

  test("Part 1 examples") {
    //CPU(parseInstructions(exampleInput)).run().foreach(println)
    assert(sumSignalStrengths(parseInstructions(exampleInput2)) == 13140)
  }

  test("Part 1 input answer") {
    assert(sumSignalStrengths(parseInstructions(input)) == 11960)
  }

  test("Part 2 examples") {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      render(parseInstructions(exampleInput2))
    }
    assert(out.toString.trim ==
      """##..##..##..##..##..##..##..##..##..##..
        |###...###...###...###...###...###...###.
        |####....####....####....####....####....
        |#####.....#####.....#####.....#####.....
        |######......######......######......####
        |#######.......#######.......#######.....""".stripMargin)
  }

  test("Part 2 input answer") {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      render(parseInstructions(input)) // EJCFPGLH
    }
    assert(out.toString.trim ==
      """####...##..##..####.###...##..#....#..#.
        |#.......#.#..#.#....#..#.#..#.#....#..#.
        |###.....#.#....###..#..#.#....#....####.
        |#.......#.#....#....###..#.##.#....#..#.
        |#....#..#.#..#.#....#....#..#.#....#..#.
        |####..##...##..#....#.....###.####.#..#.""".stripMargin)
  }
}
