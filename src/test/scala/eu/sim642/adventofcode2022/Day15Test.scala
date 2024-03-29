package eu.sim642.adventofcode2022

import Day15.*
import Day15Test.*
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day15Test extends Suites(
  new Part1Test,
  new SeminaivePart2SolutionTest,
  new BoxPart2SolutionTest,
)

object Day15Test {

  val exampleInput =
    """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
      |Sensor at x=9, y=16: closest beacon is at x=10, y=16
      |Sensor at x=13, y=2: closest beacon is at x=15, y=3
      |Sensor at x=12, y=14: closest beacon is at x=10, y=16
      |Sensor at x=10, y=20: closest beacon is at x=10, y=16
      |Sensor at x=14, y=17: closest beacon is at x=10, y=16
      |Sensor at x=8, y=7: closest beacon is at x=2, y=10
      |Sensor at x=2, y=0: closest beacon is at x=2, y=10
      |Sensor at x=0, y=11: closest beacon is at x=2, y=10
      |Sensor at x=20, y=14: closest beacon is at x=25, y=17
      |Sensor at x=17, y=20: closest beacon is at x=21, y=22
      |Sensor at x=16, y=7: closest beacon is at x=15, y=3
      |Sensor at x=14, y=3: closest beacon is at x=15, y=3
      |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin

  class Part1Test extends AnyFunSuite {

    test("Part 1 examples") {
      assert(countNoBeaconY(parseSensorBeacons(exampleInput), 10) == 26)
    }

    test("Part 1 input answer") {
      assert(countNoBeaconY(parseSensorBeacons(input)) == 4748135)
    }
  }

  abstract class Part2SolutionTest(part2Solution: Part2Solution) extends AnyFunSuite {

    test("Part 2 examples") {
      assert(part2Solution.tuningFrequency(parseSensorBeacons(exampleInput), 20) == 56000011)
    }

    test("Part 2 input answer") {
      assert(part2Solution.tuningFrequency(parseSensorBeacons(input)) == 13743542639657L)
    }
  }

  class SeminaivePart2SolutionTest extends Part2SolutionTest(SeminaivePart2Solution)

  class BoxPart2SolutionTest extends Part2SolutionTest(BoxPart2Solution)
}
