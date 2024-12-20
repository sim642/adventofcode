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
    val cheats = Part1.findCheats(parseGrid(exampleInput)).groupCount(_.save)
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
    assert(Part1.countGoodCheats(parseGrid(input)) == 1490)
  }

  test("Part 2 examples") {
    val cheats = Part2.findCheats(parseGrid(exampleInput)).groupCount(_.save)
    assert(cheats(50) == 32)
    assert(cheats(52) == 31)
    assert(cheats(54) == 29)
    assert(cheats(56) == 39)
    assert(cheats(58) == 25)
    assert(cheats(60) == 23)
    assert(cheats(62) == 20)
    assert(cheats(64) == 19)
    assert(cheats(66) == 12)
    assert(cheats(68) == 14)
    assert(cheats(70) == 12)
    assert(cheats(72) == 22)
    assert(cheats(74) == 4)
    assert(cheats(76) == 3)
  }

  ignore("Part 2 input answer") { // TODO: optimize (~4.3s)
    assert(Part2.countGoodCheats(parseGrid(input)) == 1011325)
  }
}
