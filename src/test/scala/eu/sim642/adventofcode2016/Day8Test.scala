package eu.sim642.adventofcode2016

import Day8._
import org.scalatest.funsuite.AnyFunSuite

import java.io.ByteArrayOutputStream

class Day8Test extends AnyFunSuite {

  test("Part 1 input answer") {
    assert(litPixels(input) == 106)
  }

  test("Part 2 input answer") {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      render(input) // CFLELOYFCS
    }
    assert(out.toString.trim ==
      """.##..####.#....####.#.....##..#...#####..##...###.
        |#..#.#....#....#....#....#..#.#...##....#..#.#....
        |#....###..#....###..#....#..#..#.#.###..#....#....
        |#....#....#....#....#....#..#...#..#....#.....##..
        |#..#.#....#....#....#....#..#...#..#....#..#....#.
        |.##..#....####.####.####..##....#..#.....##..###..""".stripMargin)
  }
}
