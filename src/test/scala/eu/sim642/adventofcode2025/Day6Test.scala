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
    assert(sumAnswers(parseProblems(exampleInput)) == 4277556)
  }

  test("Part 1 input answer") {
    assert(sumAnswers(parseProblems(input)) == 5361735137219L)
  }
}
