package eu.sim642.adventofcode2023

import Day7._
import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite {

  private val exampleInput =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin

  test("Part 1 examples") {
    import Part1._

    assert(handType(parseHand("AAAAA")) == HandType.FiveOfAKind)
    assert(handType(parseHand("AA8AA")) == HandType.FourOfAKind)
    assert(handType(parseHand("23332")) == HandType.FullHouse)
    assert(handType(parseHand("TTT98")) == HandType.ThreeOfAKind)
    assert(handType(parseHand("23432")) == HandType.TwoPair)
    assert(handType(parseHand("A23A4")) == HandType.OnePair)
    assert(handType(parseHand("23456")) == HandType.HighCard)

    assert(cardOrdering.lt('2', 'A'))
    assert(handTypeOrdering.lt(HandType.HighCard, HandType.FiveOfAKind))

    assert(totalWinnings(parseHands(exampleInput)) == 6440)
  }

  test("Part 1 input answer") {
    assert(Part1.totalWinnings(parseHands(input)) == 247815719)
  }

  test("Part 2 examples") {
    import Part2._

    assert(handType(parseHand("QJJQ2")) == HandType.FourOfAKind)

    assert(handType(parseHand("32T3K")) == HandType.OnePair)
    assert(handType(parseHand("KK677")) == HandType.TwoPair)
    assert(handType(parseHand("T55J5")) == HandType.FourOfAKind)
    assert(handType(parseHand("KTJJT")) == HandType.FourOfAKind)
    assert(handType(parseHand("QQQJA")) == HandType.FourOfAKind)

    assert(cardOrdering.lt('J', '2'))
    assert(handOrdering.lt(parseHand("JKKK2"), parseHand("QQQQ2")))

    assert(totalWinnings(parseHands(exampleInput)) == 5905)
  }

  test("Part 2 input answer") {
    assert(Part2.totalWinnings(parseHands(input)) == 248747492)
  }
}
