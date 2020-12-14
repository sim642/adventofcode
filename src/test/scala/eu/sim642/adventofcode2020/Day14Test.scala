package eu.sim642.adventofcode2020

import Day14._
import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite {

  val exampleInput =
    """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
      |mem[8] = 11
      |mem[7] = 101
      |mem[8] = 0""".stripMargin

  val exampleInput2 =
    """mask = 000000000000000000000000000000X1001X
      |mem[42] = 100
      |mask = 00000000000000000000000000000000X0XX
      |mem[26] = 1""".stripMargin

  test("Part 1 examples") {
    assert(Part1.sumFinalMemory(parseInstructions(exampleInput)) == 165)
  }

  test("Part 1 input answer") {
    assert(Part1.sumFinalMemory(parseInstructions(input)) == 10452688630537L)
  }

  test("Part 2 examples") {
    assert(Part2.sumFinalMemory(parseInstructions(exampleInput2)) == 208)
  }

  test("Part 2 input answer") {
    assert(Part2.sumFinalMemory(parseInstructions(input)) == 2881082759597L)
  }
}
