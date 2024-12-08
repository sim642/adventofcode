package eu.sim642.adventofcode2024

import Day8._
import org.scalatest.funsuite.AnyFunSuite

class Day8Test extends AnyFunSuite {

  val exampleInput =
    """............
      |........0...
      |.....0......
      |.......0....
      |....0.......
      |......A.....
      |............
      |............
      |........A...
      |.........A..
      |............
      |............""".stripMargin

  test("Part 1 examples") {
    assert(countAntinodes(parseGrid(exampleInput)) == 14)
  }

  test("Part 1 input answer") {
    assert(countAntinodes(parseGrid(input)) == 357)
  }
}
