package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day11._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day11Test extends FunSuite with ScalaCheckPropertyChecks {

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
}
