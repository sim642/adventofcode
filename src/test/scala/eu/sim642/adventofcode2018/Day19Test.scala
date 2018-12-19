package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day19._

class Day19Test extends FunSuite {

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
    assert(runProgram(exampleInput, 0) == 6)
  }

  test("Part 1 input answer") {
    assert(runProgram(input, 0) == 1120)
  }
}
