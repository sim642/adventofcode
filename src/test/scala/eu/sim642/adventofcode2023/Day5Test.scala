package eu.sim642.adventofcode2023

import Day5._
import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {

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

    val seed2soil = input.rangeMaps.head
    assert(seed2soil(Interval(79)) == Set(Interval(81)))
    assert(seed2soil(Interval(14)) == Set(Interval(14)))
    assert(seed2soil(Interval(55)) == Set(Interval(57)))
    assert(seed2soil(Interval(13)) == Set(Interval(13)))

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