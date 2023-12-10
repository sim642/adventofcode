package eu.sim642.adventofcode2023

import Day10._
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {

  private val exampleInput =
    """.....
      |.S-7.
      |.|.|.
      |.L-J.
      |.....""".stripMargin

  private val exampleInput2 =
    """-L|F7
      |7S-7|
      |L|7||
      |-L-J|
      |L|-JF""".stripMargin

  private val exampleInput3 =
    """..F7.
      |.FJ|.
      |SJ.L7
      ||F--J
      |LJ...""".stripMargin

  private val exampleInput4 =
    """7-F7-
      |.FJ|7
      |SJLL7
      ||F--J
      |LJ.LJ""".stripMargin

  test("Part 1 examples") {
    assert(farthestDistance(parseGrid(exampleInput)) == 4)
    assert(farthestDistance(parseGrid(exampleInput2)) == 4)
    assert(farthestDistance(parseGrid(exampleInput3)) == 8)
    assert(farthestDistance(parseGrid(exampleInput4)) == 8)
  }

  test("Part 1 input answer") {
    assert(farthestDistance(parseGrid(input)) == 7063)
  }
}
