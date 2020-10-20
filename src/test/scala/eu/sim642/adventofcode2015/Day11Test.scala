package eu.sim642.adventofcode2015

import Day11._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("Part 1 examples") {
    val increments = Table(
      "s",
      "xx",
      "xy",
      "xz",
      "ya",
      "yb",
    )

    val it = iterateIncrements("xx")
    forAll (increments) { expected =>
      assert(it.next() == expected)
    }


    assert(req1("hijklmmn"))
    assert(!req2("hijklmmn"))
    assert(req3("abbceffg"))
    assert(!req1("abbceffg"))
    assert(!req3("abbcegjk"))

    assert(findPassword("abcdefgh") == "abcdffaa")
    assert(findPassword("ghijklmn") == "ghjaabcc")
  }

  test("Part 1 input answer") {
    assert(findPassword(input) == "hxbxxyzz")
  }

  test("Part 2 input answer") {
    assert(findPassword(input, 1) == "hxcaabcc")
  }
}
