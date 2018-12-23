package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day23._

class Day23Test extends FunSuite {

  val exampleInput1 =
    """pos=<0,0,0>, r=4
      |pos=<1,0,0>, r=1
      |pos=<4,0,0>, r=3
      |pos=<0,2,0>, r=1
      |pos=<0,5,0>, r=3
      |pos=<0,0,3>, r=1
      |pos=<1,1,1>, r=1
      |pos=<1,1,2>, r=1
      |pos=<1,3,1>, r=1""".stripMargin

  val exampleInput2 =
    """pos=<10,12,12>, r=2
      |pos=<12,14,12>, r=2
      |pos=<16,12,12>, r=4
      |pos=<14,14,14>, r=6
      |pos=<50,50,50>, r=200
      |pos=<10,10,10>, r=5""".stripMargin

  test("Part 1 examples") {
    assert(nanobotsInLargestRadius(parseInput(exampleInput1)) == 7)
  }

  test("Part 1 input answer") {
    assert(nanobotsInLargestRadius(parseInput(input)) == 219)
  }

  test("Part 2 examples") {
    assert(closestMostNanobots(parseInput(exampleInput2)) == 36)
  }

  test("Part 2 input answer") {
    assert(closestMostNanobots(parseInput(input)) == 83779034)
  }
}
