package eu.sim642.adventofcode2016

import Day7._
import org.scalatest.FunSuite

class Day7Test extends FunSuite {

  test("Part 1 examples") {
    assert(supportsTLS("abba[mnop]qrst"))
    assert(!supportsTLS("abcd[bddb]xyyx"))
    assert(!supportsTLS("aaaa[qwer]tyui"))
    assert(supportsTLS("ioxxoj[asdfgh]zxcvbn"))
  }

  test("Part 1 input answer") {
    assert(countSupportsTLS(input) == 115)
  }
}
