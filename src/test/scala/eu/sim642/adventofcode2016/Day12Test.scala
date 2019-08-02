package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import Day12._

class Day12Test extends FunSuite {

  val exampleInput =
    """cpy 41 a
      |inc a
      |inc a
      |dec a
      |jnz a 2
      |dec a""".stripMargin

  test("Part 1 examples") {
    assert(execRegisterA(exampleInput) == 42)
  }

  test("Part 1 input answer") {
    assert(execRegisterA(input) == 318020)
  }
}
