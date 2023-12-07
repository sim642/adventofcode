package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.IterableImplicits.*

import scala.math.Ordering.IntOrdering

object Day7 {

  type Card = Char
  type Hand = Seq[Card]

  enum HandType {
    case FiveOfAKind
    case FourOfAKind
    case FullHouse
    case ThreeOfAKind
    case TwoPair
    case OnePair
    case HighCard
  }

  val handTypeOrdering: Ordering[HandType] = Ordering.by[HandType, Int](_.ordinal)(Ordering[Int]).reverse

  trait Part {
    protected val cardStrengthOrder: Seq[Card]

    val cardOrdering: Ordering[Card] = Ordering.by(cardStrengthOrder.indexOf).reverse

    def handType(hand: Hand): HandType

    val handOrdering: Ordering[Hand] = Ordering.by(handType)(handTypeOrdering).orElse(Ordering.Implicits.seqOrdering(cardOrdering))

    def totalWinnings(hands: Seq[(Hand, Int)]): Int = {
      hands
        .sortBy(_._1)(handOrdering)
        .zipWithIndex
        .map({ case ((hand, bid), i) => (i + 1) * bid })
        .sum
    }
  }

  object Part1 extends Part {
    override protected val cardStrengthOrder: Seq[Card] =
      Seq('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')

    override def handType(hand: Hand): HandType = hand.groupCount(identity).values.toSeq.sorted(Ordering[Int].reverse) match {
      case Seq(5) => HandType.FiveOfAKind
      case Seq(4, 1) => HandType.FourOfAKind
      case Seq(3, 2) => HandType.FullHouse
      case Seq(3, 1, 1) => HandType.ThreeOfAKind
      case Seq(2, 2, 1) => HandType.TwoPair
      case Seq(2, 1, 1, 1) => HandType.OnePair
      case Seq(1, 1, 1, 1, 1) => HandType.HighCard
      case _ => throw IllegalArgumentException("invalid hand")
    }
  }

  object Part2 extends Part {
    override protected val cardStrengthOrder: Seq[Card] =
      Seq('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J')

    override def handType(hand: Hand): HandType = {
      val jokers = hand.count(_ == 'J')
      val handWithoutJokers = hand.filter(_ != 'J')
      // TODO: simplify, is complete?
      (jokers, handWithoutJokers.groupCount(identity).values.toSeq.sorted(Ordering[Int].reverse)) match {
        case (5, Seq()) => HandType.FiveOfAKind
        case (_, Seq(_)) => HandType.FiveOfAKind

        case (3, Seq(1, 1)) => HandType.FourOfAKind
        case (2, Seq(2, 1)) => HandType.FourOfAKind
        case (1, Seq(3, 1)) => HandType.FourOfAKind
        case (0, Seq(4, 1)) => HandType.FourOfAKind

        case (1, Seq(2, 2)) => HandType.FullHouse
        case (0, Seq(3, 2)) => HandType.FullHouse

        case (2, Seq(1, 1, 1)) => HandType.ThreeOfAKind
        case (1, Seq(2, 1, 1)) => HandType.ThreeOfAKind
        case (0, Seq(3, 1, 1)) => HandType.ThreeOfAKind

        case (0, Seq(2, 2, 1)) => HandType.TwoPair

        case (1, Seq(1, 1, 1, 1)) => HandType.OnePair
        case (0, Seq(2, 1, 1, 1)) => HandType.OnePair

        case (0, Seq(1, 1, 1, 1, 1)) => HandType.HighCard

        case _ => throw IllegalArgumentException(s"invalid hand $hand")
      }
    }
  }


  def parseHand(s: String): Hand = s.toSeq

  def parseHandBid(s: String): (Hand, Int) = s match {
    case s"$hand $bid" => parseHand(hand) -> bid.toInt
  }

  def parseHands(input: String): Seq[(Hand, Int)] = input.linesIterator.map(parseHandBid).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.totalWinnings(parseHands(input)))
    println(Part2.totalWinnings(parseHands(input)))
  }
}
