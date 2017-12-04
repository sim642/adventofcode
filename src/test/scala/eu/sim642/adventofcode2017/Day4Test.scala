package eu.sim642.adventofcode2017

import Day4._
import org.scalatest.FunSuite

class Day4Test extends FunSuite {

  test("Part 1 examples") {
    assert(isValidPassphrase("aa bb cc dd ee"))
    assert(!isValidPassphrase("aa bb cc dd aa"))
    assert(isValidPassphrase("aa bb cc dd aaa"))
  }

  test("Part 1 input answer") {
    assert(countValidPassphrases(inputLines) == 325)
  }
}
