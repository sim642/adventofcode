package eu.sim642.adventofcode2023

import Day7.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day7Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  private val exampleInput =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin

  test("Part 1 handType") {
    val handExpectedType = Table(
      ("hand", "expectedType"),
      ("AAAAA", HandType.FiveOfAKind),
      ("AA8AA", HandType.FourOfAKind),
      ("23332", HandType.FullHouse),
      ("TTT98", HandType.ThreeOfAKind),
      ("23432", HandType.TwoPair),
      ("A23A4", HandType.OnePair),
      ("23456", HandType.HighCard),
    )

    forAll(handExpectedType) { (hand, expectedType) =>
      assert(Part1.handType(parseHand(hand)) == expectedType)
    }
  }

  test("Part 1 ordering") {
    import Part1._

    assert(cardOrdering.lt('2', 'A'))
    assert(handTypeOrdering.lt(HandType.HighCard, HandType.FiveOfAKind))
  }

  test("Part 1 examples") {
    assert(Part1.totalWinnings(parseHands(exampleInput)) == 6440)
  }

  test("Part 1 input answer") {
    assert(Part1.totalWinnings(parseHands(input)) == 247815719)
  }

  test("Part 2 handType") {
    val handExpectedType = Table(
      ("hand", "expectedType"),
      ("QJJQ2", HandType.FourOfAKind),
      ("32T3K", HandType.OnePair),
      ("KK677", HandType.TwoPair),
      ("T55J5", HandType.FourOfAKind),
      ("KTJJT", HandType.FourOfAKind),
      ("QQQJA", HandType.FourOfAKind),
    )

    forAll(handExpectedType) { (hand, expectedType) =>
      assert(Part2.handType(parseHand(hand)) == expectedType)
    }
  }

  test("Part 2 ordering") {
    import Part2._

    assert(cardOrdering.lt('J', '2'))
    assert(handOrdering.lt(parseHand("JKKK2"), parseHand("QQQQ2")))
  }

  test("Part 2 examples") {
    assert(Part2.totalWinnings(parseHands(exampleInput)) == 5905)
  }

  test("Part 2 input answer") {
    assert(Part2.totalWinnings(parseHands(input)) == 248747492)
  }
}
