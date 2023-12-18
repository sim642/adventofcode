package eu.sim642.adventofcode2023

import Day5.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day5Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  private val exampleInput =
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4""".stripMargin

  test("Part 1 examples") {
    val input = parseInput(exampleInput)

    val seed2expectedSoil = Table(
      ("seed", "expectedSoil"),
      (79, 81),
      (14, 14),
      (55, 57),
      (13, 13),
    )

    val seed2soil = input.rangeMaps.head
    forAll(seed2expectedSoil) { (seed, expectedSoil) =>
      assert(seed2soil(Interval(seed)) == Set(Interval(expectedSoil)))
    }

    assert(lowestSeedLocation(input) == 35)
  }

  test("Part 1 input answer") {
    assert(lowestSeedLocation(parseInput(input)) == 226172555)
  }

  test("Part 2 examples") {
    assert(lowestSeedRangeLocation(parseInput(exampleInput)) == 46)
  }

  test("Part 2 input answer") {
    assert(lowestSeedRangeLocation(parseInput(input)) == 47909639)
  }
}
