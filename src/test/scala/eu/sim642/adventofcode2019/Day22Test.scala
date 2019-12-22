package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day22._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day22Test extends FunSuite with ScalaCheckPropertyChecks {

  val exampleInput =
    """deal with increment 7
      |deal into new stack
      |deal into new stack""".stripMargin

  val exampleInput2 =
    """cut 6
      |deal with increment 7
      |deal into new stack""".stripMargin

  val exampleInput3 =
    """deal with increment 7
      |deal with increment 9
      |cut -2""".stripMargin

  val exampleInput4 =
    """deal into new stack
      |cut -2
      |deal with increment 7
      |cut 8
      |cut -4
      |deal with increment 7
      |cut 3
      |deal with increment 9
      |deal with increment 3
      |cut -1""".stripMargin

  test("Part 1 examples") {
    val inputExpectedDeck = Table(
      ("input", "expectedDeck"),
      ("deal into new stack", Seq(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)),
      ("cut 3", Seq(3, 4, 5, 6, 7, 8, 9, 0, 1, 2)),
      ("deal with increment 3", Seq(0, 7, 4, 1, 8, 5, 2, 9, 6, 3)),
      (exampleInput, Seq(0, 3, 6, 9, 2, 5, 8, 1, 4, 7)),
      (exampleInput2, Seq(3, 0, 7, 4, 1, 8, 5, 2, 9, 6)),
      (exampleInput3, Seq(6, 3, 0, 7, 4, 1, 8, 5, 2, 9)),
      (exampleInput4, Seq(9, 2, 5, 8, 1, 4, 7, 0, 3, 6)),
    )

    forAll (inputExpectedDeck) { (input, expectedDeck) =>
      assert(shuffleFactoryOrder(parseTechniques(input), 10) == expectedDeck)
    }
  }

  test("Part 1 input answer") {
    assert(shuffleFactoryOrderPosition(parseTechniques(input)) == 7665)
  }
}
