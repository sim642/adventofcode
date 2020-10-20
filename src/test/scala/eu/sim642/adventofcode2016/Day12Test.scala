package eu.sim642.adventofcode2016

import Day12._
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {

  val exampleInput =
    """cpy 41 a
      |inc a
      |inc a
      |dec a
      |jnz a 2
      |dec a""".stripMargin

  test("Part 1 examples") {
    assert(Part1.execRegisterA(exampleInput) == 42)
  }

  test("Part 1 input answer") {
    assert(Part1.execRegisterA(input) == 318020)
  }

  test("Part 2 input answer") {
    assert(Part2.execRegisterA(input) == 9227674)
  }
}
