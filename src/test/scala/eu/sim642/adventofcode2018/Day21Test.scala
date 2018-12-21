package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day21._

class Day21Test extends FunSuite {

  import ReverseEngineeredSolution._

  test("Part 1 input answer") {
    assert(firstHaltr0(input) == 13970209)
  }

  test("Part 2 input answer") {
    assert(lastHaltr0(input) == 6267260)
  }
}
