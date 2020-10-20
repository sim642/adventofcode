package eu.sim642.adventofcode2018

import Day19._
import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite {

  val exampleInput =
    """#ip 0
      |seti 5 0 1
      |seti 6 0 2
      |addi 0 1 0
      |addr 1 2 3
      |setr 1 0 0
      |seti 8 0 4
      |seti 9 0 5""".stripMargin

  test("Part 1 examples") {
    assert(runProgram(exampleInput) == 6)
  }

  test("Part 1 input answer") {
    assert(runProgram(input) == 1120)
  }

  test("Part 2 examples for part 1") {
    assert(sumDivisors(inputN1) == 1120)
  }

  test("Part 2 input answer") {
    assert(sumDivisors(inputN2) == 12768192)
  }
}
