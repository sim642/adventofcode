package eu.sim642.adventofcode2024

import Day20._
import org.scalatest.funsuite.AnyFunSuite
import eu.sim642.adventofcodelib.IterableImplicits._

class Day20Test extends AnyFunSuite {

  val exampleInput =
    """###############
      |#...#...#.....#
      |#.#.#.#.#.###.#
      |#S#...#.#.#...#
      |#######.#.#.###
      |#######.#.#...#
      |#######.#.###.#
      |###..E#...#...#
      |###.#######.###
      |#...###...#...#
      |#.#####.#.###.#
      |#.#...#.#.#...#
      |#.#.#.#.#.#.###
      |#...#...#...###
      |###############""".stripMargin

  test("Part 1 examples") {
    val cheats = findCheats(parseGrid(exampleInput)).groupCount(_.save)
    assert(cheats(2) == 14)
    assert(cheats(4) == 14)
    assert(cheats(6) == 2)
    assert(cheats(8) == 4)
    assert(cheats(10) == 2)
    assert(cheats(12) == 3)
    assert(cheats(20) == 1)
    assert(cheats(36) == 1)
    assert(cheats(38) == 1)
    assert(cheats(40) == 1)
    assert(cheats(64) == 1)
  }

  test("Part 1 input answer") {
    assert(countGoodCheats(parseGrid(input)) == 1490)
  }
}
