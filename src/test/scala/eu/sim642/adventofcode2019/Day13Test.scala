package eu.sim642.adventofcode2019

import Day13._
import intcode.parseProgram
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(renderOutputs(LazyList(1L,2,3,6,5,4)) ==
      Map(
        Pos(1, 2) -> 3,
        Pos(6, 5) -> 4
      )
    )
  }

  test("Part 1 input answer") {
    assert(countBlocks(parseProgram(input)) == 247)
  }

  test("Part 2 input answer") {
    assert(playGame(parseProgram(input)) == 12954)
  }
}
