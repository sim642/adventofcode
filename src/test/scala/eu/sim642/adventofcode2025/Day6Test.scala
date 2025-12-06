package eu.sim642.adventofcode2025

import Day6._
import org.scalatest.funsuite.AnyFunSuite

class Day6Test extends AnyFunSuite {

  val exampleInput =
    """123 328  51 64
      | 45 64  387 23
      |  6 98  215 314
      |*   +   *   +""".stripMargin

  test("Part 1 examples") {
    assert(sumAnswers(Part1.parseProblems(exampleInput)) == 4277556)
  }

  test("Part 1 input answer") {
    assert(sumAnswers(Part1.parseProblems(input)) == 5361735137219L)
  }

  test("Part 2 examples") {
    assert(sumAnswers(Part2.parseProblems(exampleInput)) == 3263827)
  }

  test("Part 2 input answer") {
    assert(sumAnswers(Part2.parseProblems(input)) == 11744693538946L)
  }
}
