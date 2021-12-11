package eu.sim642.adventofcode2021

import Day11._
import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite {

  val exampleInput1 =
    """11111
      |19991
      |19191
      |19991
      |11111""".stripMargin

  val exampleInput2 =
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526""".stripMargin

  test("Part 1 examples") {
    assert(simulateStep(parseGrid(exampleInput1))._1 == parseGrid(
      """34543
        |40004
        |50005
        |40004
        |34543""".stripMargin
    ))

    assert(countFlashes(parseGrid(exampleInput1), 1) == 9)
    assert(countFlashes(parseGrid(exampleInput2), 10) == 204)
    assert(countFlashes(parseGrid(exampleInput2), 100) == 1656)
  }

  test("Part 1 input answer") {
    assert(countFlashes(parseGrid(input), 100) == 1773)
  }

  test("Part 2 examples") {
    assert(findSimultaneousFlash(parseGrid(exampleInput2)) == 195)
  }

  test("Part 2 input answer") {
    assert(findSimultaneousFlash(parseGrid(input)) == 494)
  }
}
