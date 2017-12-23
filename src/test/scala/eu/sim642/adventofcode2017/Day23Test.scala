package eu.sim642.adventofcode2017

import Day23._
import org.scalatest.FunSuite

class Day23Test extends FunSuite {

  test("Part 1 input answer") {
    assert(Part1.countMul(input) == 8281)
  }

  test("Primes") {
    assert(Part2.primes.take(10) == Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
  }
}
