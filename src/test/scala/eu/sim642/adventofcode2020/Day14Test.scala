package eu.sim642.adventofcode2020

import Day14._
import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite {

  val exampleInput =
    """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
      |mem[8] = 11
      |mem[7] = 101
      |mem[8] = 0""".stripMargin

  test("Part 1 examples") {
    assert(sumFinalMemory(parseInstructions(exampleInput)) == 165)
  }

  test("Part 1 input answer") {
    assert(sumFinalMemory(parseInstructions(input)) == 10452688630537L)
  }
}
