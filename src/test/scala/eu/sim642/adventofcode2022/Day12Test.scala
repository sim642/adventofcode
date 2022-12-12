package eu.sim642.adventofcode2022

import Day12._
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {

  val exampleInput =
    """Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi""".stripMargin

  test("Part 1 examples") {
    assert(fewestStepsToBestSignal(parseGrid(exampleInput)) == 31)
  }

  test("Part 1 input answer") {
    assert(fewestStepsToBestSignal(parseGrid(input)) == 423)
  }

  test("Part 2 examples") {
    assert(fewestStepsToBestSignal2(parseGrid(exampleInput)) == 29)
  }

  test("Part 2 input answer") {
    assert(fewestStepsToBestSignal2(parseGrid(input)) == 416)
  }
}
