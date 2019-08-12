package eu.sim642.adventofcode2016

import Day5._
import org.scalatest.FunSuite

class Day5Test extends FunSuite {

  val exampleInput = "abc"

  // ignored because very slow (30s total)

  ignore("Part 1 examples") {
    assert(getPassword(exampleInput, 3) == "18f")
    assert(getPassword(exampleInput) == "18f47a30")
  }

  ignore("Part 1 input answer") {
    assert(getPassword(input) == "801b56a7")
  }

  ignore("Part 2 examples") {
    assert(getPassword2(exampleInput) == "05ace8e3")
  }

  ignore("Part 2 input answer") {
    assert(getPassword2(input) == "424a0197")
  }
}
