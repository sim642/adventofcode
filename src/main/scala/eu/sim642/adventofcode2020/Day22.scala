package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.LazyListImplicits._
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder

object Day22 {

  type Deck = Vector[Int]
  type Decks = (Deck, Deck)

  sealed trait Part {
    def playWinner(decks: Decks): Either[Deck, Deck]

    def winningScore(decks: Decks): Int = {
      val winningDeck = playWinner(decks) match {
        case Left(deck1) => deck1
        case Right(deck2) => deck2
      }
      winningDeck
        .reverseIterator
        .zipWithIndex
        .map({ case (card, i) => card * (i + 1) })
        .sum
    }
  }

  // TODO: reduce duplication

  object Part1 extends Part {

    override def playWinner(decks: Decks): Either[Deck, Deck] = {
      val roundDecks = decks #:: LazyList.unfold0(decks)(playRound) // unfold0 doesn't include first
      val (lastDeck1, lastDeck2) = roundDecks.last
      if (lastDeck1.nonEmpty)
        Left(lastDeck1)
      else
        Right(lastDeck2)
    }

    private def playRound(decks: Decks): Option[Decks] = decks match {
      case (card1 +: newDeck1, card2 +: newDeck2) =>
        if (card1 > card2)
          Some((newDeck1 :+ card1 :+ card2, newDeck2))
        else
          Some((newDeck1, newDeck2 :+ card2 :+ card1))
      case (_, _) =>
        None
    }
  }

  object Part2 extends Part {
    // TODO: optimize

    override def playWinner(decks: Decks): Either[Deck, Deck] = {
      val roundDecks = decks #:: LazyList.unfold0(decks)(playRound) // unfold0 doesn't include first
      NaiveCycleFinder.find(roundDecks) match {
        case None =>
          val (lastDeck1, lastDeck2) = roundDecks.last
          if (lastDeck1.nonEmpty)
            Left(lastDeck1)
          else
            Right(lastDeck2)
        case Some(cycle) =>
          Left(cycle.cycleHead._1)
      }
    }

    private def playRound(decks: Decks): Option[Decks] = decks match {
      case (card1 +: newDeck1, card2 +: newDeck2) =>
        val winner: Either[Any, Any] = {
          if (newDeck1.lengthIs >= card1 && newDeck2.lengthIs >= card2) {
            val recDecks = (newDeck1.take(card1), newDeck2.take(card2))
            playWinner(recDecks)
          } else {
            if (card1 > card2)
              Left()
            else
              Right()
          }
        }

        winner match {
          case Left(_) =>
            Some((newDeck1 :+ card1 :+ card2, newDeck2))
          case Right(_) =>
            Some((newDeck1, newDeck2 :+ card2 :+ card1))
        }
      case (_, _) =>
        None
    }
  }


  def parseDeck(s: String): Deck = s.linesIterator.drop(1).map(_.toInt).toVector // TODO: Iterator tail

  def parseDecks(input: String): Decks = {
    val Seq(s1, s2) = input.split("\n\n").toSeq
    (parseDeck(s1), parseDeck(s2))
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.winningScore(parseDecks(input)))
    println(Part2.winningScore(parseDecks(input)))
  }
}
