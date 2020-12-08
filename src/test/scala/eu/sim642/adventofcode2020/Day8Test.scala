package eu.sim642.adventofcode2020

import Day8._
import org.scalatest.funsuite.AnyFunSuite

class Day8Test extends AnyFunSuite {

  val exampleInput =
    """nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6""".stripMargin

  test("Part 1 examples") {
    assert(accBeforeLoop(parseInstructions(exampleInput)) == 5)
  }

  test("Part 1 input answer") {
    assert(accBeforeLoop(parseInstructions(input)) == 1801)
  }
}
