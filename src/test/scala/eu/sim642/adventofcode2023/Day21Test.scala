package eu.sim642.adventofcode2023

import Day21._
import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends AnyFunSuite {

  private val exampleInput =
    """...........
      |.....###.#.
      |.###.##..#.
      |..#.#...#..
      |....#.#....
      |.##..S####.
      |.##..#...#.
      |.......##..
      |.##.#.####.
      |.##..##.##.
      |...........""".stripMargin

  test("Part 1 examples") {
    assert(countReachableExactly(parseGrid(exampleInput), 6) == 16)
  }

  test("Part 1 input answer") {
    assert(countReachableExactly(parseGrid(input)) == 3687)
  }

  test("Part 2 examples") {
    //assert(countReachableExactlyInfinite(parseGrid(exampleInput), 6) == 16)
    //assert(countReachableExactlyInfinite(parseGrid(exampleInput), 10) == 50)
    //assert(countReachableExactlyInfinite(parseGrid(exampleInput), 50) == 1594)
    //assert(countReachableExactlyInfinite(parseGrid(exampleInput), 100) == 6536)
    //assert(countReachableExactlyInfinite(parseGrid(exampleInput), 500) == 167004)
    //assert(countReachableExactlyInfinite(parseGrid(exampleInput), 1000) == 668697)
    //assert(countReachableExactlyInfinite(parseGrid(exampleInput), 5000) == 16733044)
  }

  test("Part 2 input answer") {
    //assert(countReachableExactlyInfinite(parseGrid(input), 64) == 3687)
    //assert(countReachableExactlyInfinite(parseGrid(input), 128) == 14452)
    //assert(countReachableExactlyInfinite(parseGrid(input), 256) == 57375)
    //assert(countReachableExactlyInfinite(parseGrid(input), 512) == 228690)
    //assert(countReachableExactlyInfinite(parseGrid(input), 1024) == 912913)
    //println(Day9.Part1.extrapolate(Seq(15130, 60085, 134866, 239473)))

    //assert(countReachableExactlyInfinite(parseGrid(input), 131) == 15130)
    //assert(countReachableExactlyInfinite(parseGrid(input), 2 * 131) == 60085)
    //assert(countReachableExactlyInfinite(parseGrid(input), 3 * 131) == 134866)
    //assert(countReachableExactlyInfinite(parseGrid(input), 4 * 131) == 239473)
    //assert(countReachableExactlyInfinite(parseGrid(input), 5 * 131) == 373906)


    //assert(countReachableExactlyInfinite2(parseGrid(input), 64) == 3687)
    //assert(countReachableExactlyInfinite2(parseGrid(input), 128) == 14452)
    //assert(countReachableExactlyInfinite2(parseGrid(input), 256) == 57375)
    //assert(countReachableExactlyInfinite2(parseGrid(input), 512) == 228690)
    //assert(countReachableExactlyInfinite2(parseGrid(input), 1024) == 912913)

    assert(countReachableExactlyInfinite2(parseGrid(input)) == 610321885082978L)
  }
}
