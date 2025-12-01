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

  test("Part 1 examples") {
    assert(Part1.password(parseRotations(exampleInput)) == 3)
  }

  test("Part 1 input answer") {
    assert(Part1.password(parseRotations(input)) == 995)
  }

  test("Part 2 examples") {
    assert(Part2.password(parseRotations(exampleInput)) == 6)
    assert(Part2.password(parseRotations("R1000")) == 10)
  }

  test("Part 2 input answer") {
    assert(Part2.password(parseRotations(input)) == 5847)
  }
}
