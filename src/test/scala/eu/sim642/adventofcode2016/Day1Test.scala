package eu.sim642.adventofcode2016

import Day1._
import org.scalatest.FunSuite

class Day1Test extends FunSuite {

  test("parseInstructions") {
    assert(parseInstructions("R2, L3") == Seq(Right(2), Left(3)))
  }

  test("Part 1 examples") {
    assert(shortestDestinationDist(Seq(Right(2), Left(3))) == 5)
    assert(shortestDestinationDist(Seq(Right(2), Right(2), Right(2))) == 2)
    assert(shortestDestinationDist(Seq(Right(5), Left(5), Right(5), Right(3))) == 12)
  }

  test("Part 1 input answer") {
    assert(shortestDestinationDist(input) == 209)
  }
}
