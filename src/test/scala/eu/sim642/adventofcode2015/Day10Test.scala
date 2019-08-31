package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day10._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day10Test extends FunSuite with ScalaCheckPropertyChecks {

  test("Part 1 examples") {
    val seqs = Table(
      "seq",
      "1",
      "11",
      "21",
      "1211",
      "111221",
      "312211",
    )

    val it = lookAndSayIterator("1")
    forAll (seqs) { seq =>
      assert(it.next() == seq)
    }
  }

  test("Part 1 input answer") {
    assert(lookAndSayLength(input, 40) == 492982)
  }

  test("Part 2 input answer") {
    assert(lookAndSayLength(input, 50) == 6989950)
  }
}
