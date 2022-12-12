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
    assert(Part1.fewestStepsToBestSignal(parseGrid(exampleInput)) == 31)
  }

  test("Part 1 input answer") {
    assert(Part1.fewestStepsToBestSignal(parseGrid(input)) == 423)
  }

  test("Part 2 examples") {
    assert(Part2.fewestStepsToBestSignal(parseGrid(exampleInput)) == 29)
  }

  test("Part 2 input answer") {
    assert(Part2.fewestStepsToBestSignal(parseGrid(input)) == 416)
  }
}
