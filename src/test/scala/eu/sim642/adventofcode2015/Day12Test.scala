package eu.sim642.adventofcode2015

import Day12._
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(Part1.sumNumbers("""[1,2,3]""") == 6)
    assert(Part1.sumNumbers("""{"a":2,"b":4}""") == 6)
    assert(Part1.sumNumbers("""[[[3]]]""") == 3)
    assert(Part1.sumNumbers("""{"a":{"b":4},"c":-1}""") == 3)
    assert(Part1.sumNumbers("""{"a":[-1,1]}""") == 0)
    assert(Part1.sumNumbers("""[-1,{"a":1}]""") == 0)
    assert(Part1.sumNumbers("""[]""") == 0)
    assert(Part1.sumNumbers("""{}""") == 0)
  }

  test("Part 1 input answer") {
    assert(Part1.sumNumbers(input) == 119433)
  }

  test("Part 2 examples") {
    assert(Part2.sumNumbers("""[1,2,3]""") == 6)
    assert(Part2.sumNumbers("""[1,{"c":"red","b":2},3]""") == 4)
    assert(Part2.sumNumbers("""{"d":"red","e":[1,2,3,4],"f":5}""") == 0)
    assert(Part2.sumNumbers("""[1,"red",5]""") == 6)
  }

  test("Part 2 input answer") {
    assert(Part2.sumNumbers(input) == 68466)
  }
}
