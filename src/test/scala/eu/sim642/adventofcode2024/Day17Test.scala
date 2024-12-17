package eu.sim642.adventofcode2024

import Day17._
import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {

  // TODO: small examples

  val exampleInput =
    """Register A: 729
      |Register B: 0
      |Register C: 0
      |
      |Program: 0,1,5,4,3,0""".stripMargin

  val exampleInput1 =
    """Register A: 10
      |Register B: 0
      |Register C: 0
      |
      |Program: 5,0,5,1,5,4""".stripMargin

  val exampleInput2 =
    """Register A: 2024
      |Register B: 0
      |Register C: 0
      |
      |Program: 0,1,5,4,3,0""".stripMargin

  test("Part 1 examples") {
    assert(runOutput(parseInput(exampleInput)) == "4,6,3,5,6,3,5,2,1,0")
    assert(runOutput(parseInput(exampleInput1)) == "0,1,2")
    assert(runOutput(parseInput(exampleInput2)) == "4,2,5,6,7,7,7,7,3,1,0")
  }

  test("Part 1 input answer") {
    assert(runOutput(parseInput(input)) == "4,3,2,6,4,5,3,2,4")
  }
}
