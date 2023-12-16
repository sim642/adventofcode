package eu.sim642.adventofcode2023

import Day16._
import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends AnyFunSuite {

  private val exampleInput =
    """.|...\....
      ||.-.\.....
      |.....|-...
      |........|.
      |..........
      |.........\
      |..../.\\..
      |.-.-/..|..
      |.|....-|.\
      |..//.|....""".stripMargin

  test("Part 1 examples") {
    assert(countEnergized(parseGrid(exampleInput)) == 46)
  }

  test("Part 1 input answer") {
    assert(countEnergized(parseGrid(input)) == 7067)
  }
}
