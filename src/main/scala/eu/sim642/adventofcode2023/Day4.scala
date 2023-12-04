package eu.sim642.adventofcode2023

import scala.annotation.tailrec

object Day4 {

  case class Card(winning: Set[Int], have: Set[Int]) {
    def haveWinning: Int = (winning intersect have).size

    def points: Int = {
      if (haveWinning == 0)
        0
      else
        1 << (haveWinning - 1)
    }
  }

  def sumPoints(cards: Seq[Card]): Int = cards.map(_.points).sum

  def countWonCards(cards: Seq[Card]): Int = {

    @tailrec
    def helper(cardCounts: Seq[(Card, Int)], acc: Int): Int = cardCounts match {
      case Seq() => acc
      case (card, count) +: newCardCounts =>
        val newCardCounts2 = (0 until card.haveWinning).foldLeft(newCardCounts)({ (newCardCounts, i) =>
          val (newCard, newCount) = newCardCounts(i)
          newCardCounts.updated(i, (newCard, newCount + count))
        })
        helper(newCardCounts2, acc + count)
    }

    helper(cards.map(_ -> 1), 0)
  }


  def parseCard(s: String): Card = s match {
    case s"Card $i: $winning | $have" =>
      Card(
        winning.trim.split(" +").map(_.toInt).toSet,
        have.trim.split(" +").map(_.toInt).toSet
      )
  }

  def parseCards(input: String): Seq[Card] = input.linesIterator.map(parseCard).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumPoints(parseCards(input)))
    println(countWonCards(parseCards(input)))
  }
}
