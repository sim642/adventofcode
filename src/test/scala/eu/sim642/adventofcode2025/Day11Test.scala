package eu.sim642.adventofcode2025

import Day11._
import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite {

  val exampleInput =
    """aaa: you hhh
      |you: bbb ccc
      |bbb: ddd eee
      |ccc: ddd eee fff
      |ddd: ggg
      |eee: out
      |fff: out
      |ggg: out
      |hhh: ccc fff iii
      |iii: out""".stripMargin

  test("Part 1 examples") {
    assert(countPaths(parseDevices(exampleInput)) == 5)
  }

  test("Part 1 input answer") {
    assert(countPaths(parseDevices(input)) == 643)
  }
}
