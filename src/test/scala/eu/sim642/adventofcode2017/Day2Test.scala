package eu.sim642.adventofcode2017

import eu.sim642.adventofcode2017.Day2._
import org.scalatest.FunSuite

class Day2Test extends FunSuite {

  test("Part 1 example") {
    assert(Part1.checksum(
      """5 1 9 5
        |7 5 3
        |2 4 6 8""".stripMargin) == 18)
  }

  test("Part 1 input answer") {
    assert(Part1.checksum(input) == 51833)
  }

  test("Part 2 example") {
    assert(Part2.checksum(
      """5 9 2 8
        |9 4 7 3
        |3 8 6 5""".stripMargin) == 9)
  }

  test("Part 2 input answer") {
    assert(Part2.checksum(input) == 288)
  }

}
