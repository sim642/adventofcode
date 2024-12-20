package eu.sim642.adventofcode2024

import Day20.*
import org.scalatest.funsuite.AnyFunSuite
import eu.sim642.adventofcodelib.IterableImplicits.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day20Test extends AnyFunSuite with ScalaCheckPropertyChecks {

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

    val saveExpectedCount = Table(
      ("save", "expectedCount"),
      (2, 14),
      (4, 14),
      (6, 2),
      (8, 4),
      (10, 2),
      (12, 3),
      (20, 1),
      (36, 1),
      (38, 1),
      (40, 1),
      (64, 1),
    )

    forAll(saveExpectedCount) { (save, expectedCount) =>
      assert(cheats(save) == expectedCount)
    }
  }

  test("Part 1 input answer") {
    assert(Part1.countGoodCheats(parseGrid(input)) == 1490)
  }

  test("Part 2 examples") {
    val cheats = Part2.findCheats(parseGrid(exampleInput)).groupCount(_.save)

    val saveExpectedCount = Table(
      ("save", "expectedCount"),
      (50, 32),
      (52, 31),
      (54, 29),
      (56, 39),
      (58, 25),
      (60, 23),
      (62, 20),
      (64, 19),
      (66, 12),
      (68, 14),
      (70, 12),
      (72, 22),
      (74, 4),
      (76, 3),
    )

    forAll(saveExpectedCount) { (save, expectedCount) =>
      assert(cheats(save) == expectedCount)
    }
  }

  test("Part 2 input answer") {
    assert(Part2.countGoodCheats(parseGrid(input)) == 1011325)
  }
}
