package eu.sim642.adventofcode2016

import Day7._
import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(supportsTLS("abba[mnop]qrst"))
    assert(!supportsTLS("abcd[bddb]xyyx"))
    assert(!supportsTLS("aaaa[qwer]tyui"))
    assert(supportsTLS("ioxxoj[asdfgh]zxcvbn"))
  }

  test("Part 1 input answer") {
    assert(countSupportsTLS(input) == 115)
  }

  test("Part 2 examples") {
    assert(supportsSSL("aba[bab]xyz"))
    assert(!supportsSSL("xyx[xyx]xyx"))
    assert(supportsSSL("aaa[kek]eke"))
    assert(supportsSSL("zazbz[bzb]cdb"))
  }

  test("Part 2 input answer") {
    assert(countSupportsSSL(input) == 231)
  }
}
