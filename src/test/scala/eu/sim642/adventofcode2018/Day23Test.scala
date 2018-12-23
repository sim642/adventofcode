package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day23._

class Day23Test extends FunSuite {

  val exampleInput =
    """pos=<0,0,0>, r=4
      |pos=<1,0,0>, r=1
      |pos=<4,0,0>, r=3
      |pos=<0,2,0>, r=1
      |pos=<0,5,0>, r=3
      |pos=<0,0,3>, r=1
      |pos=<1,1,1>, r=1
      |pos=<1,1,2>, r=1
      |pos=<1,3,1>, r=1""".stripMargin

  test("Part 1 examples") {
    assert(nanobotsInLargestRadius(parseInput(exampleInput)) == 7)
  }

  test("Part 1 input answer") {
    assert(nanobotsInLargestRadius(parseInput(input)) == 219)
  }
}
