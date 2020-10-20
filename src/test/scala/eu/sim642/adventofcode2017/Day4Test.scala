package eu.sim642.adventofcode2017

import Day4._
import org.scalatest.funsuite.AnyFunSuite

class Day4Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(Part1.isValidPassphrase("aa bb cc dd ee"))
    assert(!Part1.isValidPassphrase("aa bb cc dd aa"))
    assert(Part1.isValidPassphrase("aa bb cc dd aaa"))
  }

  test("Part 1 input answer") {
    assert(Part1.countValidPassphrases(inputLines) == 325)
  }

  test("Part 2 examples") {
    assert(Part2.isValidPassphrase("abcde fghij"))
    assert(!Part2.isValidPassphrase("abcde xyz ecdab"))
    assert(Part2.isValidPassphrase("a ab abc abd abf abj"))
    assert(Part2.isValidPassphrase("iiii oiii ooii oooi oooo"))
    assert(!Part2.isValidPassphrase("oiii ioii iioi iiio"))
  }

  test("Part 2 input answer") {
    assert(Part2.countValidPassphrases(inputLines) == 119)
  }
}
