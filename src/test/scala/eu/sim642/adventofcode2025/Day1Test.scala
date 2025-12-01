package eu.sim642.adventofcode2025

import Day1._
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {

  val exampleInput =
    """L68
      |L30
      |R48
      |L5
      |R60
      |L55
      |L1
      |L99
      |R14
      |L82""".stripMargin

  test("Part 1 example") {
    assert(actualPassword(parseRotations(exampleInput)) == 3)
  }

  test("Part 1 input answer") {
    assert(actualPassword(parseRotations(input)) == 995)
  }
}
