package eu.sim642.adventofcode2020

import scala.collection.immutable.Queue
import eu.sim642.adventofcodelib.LazyListImplicits._
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder

object Day22 {

  type Deck = Queue[Int]
  type Decks = (Deck, Deck)

  sealed trait Part {
    def playWinner(decks: Decks): Either[Decks, Decks]

    def winningScore(decks: Decks): Int = {
      val winningDeck = playWinner(decks) match {
        case Left((deck1, _)) => deck1
        case Right((_, deck2)) => deck2
      }
      winningDeck
        .reverseIterator
        .zipWithIndex
        .map({ case (card, i) => card * (i + 1) })
        .sum
    }
  }

  object Part1 extends Part {

    override def playWinner(decks: Decks): Either[Decks, Decks] = {
      val roundDecks = LazyList.unfold0(decks)(playRound)
      val lastDecks@(lastDeck1, _) = roundDecks.last
      if (lastDeck1.nonEmpty)
        Left(lastDecks)
      else
        Right(lastDecks)
    }

    def playRound(decks: Decks): Option[Decks] = {
      val (deck1, deck2) = decks
      (deck1.dequeueOption, deck2.dequeueOption) match {
        case (Some((card1, newDeck1)), Some((card2, newDeck2))) =>
          if (card1 > card2)
            Some((newDeck1.enqueue(card1).enqueue(card2), newDeck2))
          else
            Some((newDeck1, newDeck2.enqueue(card2).enqueue(card1)))
        case (_, _) =>
          None
      }
    }
  }

  object Part2 extends Part {

    override def playWinner(decks: Decks): Either[Decks, Decks] = {
      //println(s"GAME: $decks")
      val roundDecks = decks #:: LazyList.unfold0(decks)(playRound)
      NaiveCycleFinder.find(roundDecks) match {
        case None =>
          val lastDecks@(lastDeck1, _) = roundDecks.last
          //println("GAME OVER (normal)")
          if (lastDeck1.nonEmpty)
            Left(lastDecks)
          else
            Right(lastDecks)
        case Some(cycle) =>
          //println("GAME OVER (cycle)")
          Left(cycle.cycleHead)
      }
    }

    def playRound(decks: Decks): Option[Decks] = {
      //println(s"ROUND: $decks")
      val (deck1, deck2) = decks
      (deck1.dequeueOption, deck2.dequeueOption) match {
        case (Some((card1, newDeck1)), Some((card2, newDeck2))) =>
          //println(s"  $card1 vs $card2")
          // TODO: Queue has inefficient length?
          if (!(newDeck1.length >= card1 && newDeck2.length >= card2)) {
            if (card1 > card2)
              Some((newDeck1.enqueue(card1).enqueue(card2), newDeck2))
            else
              Some((newDeck1, newDeck2.enqueue(card2).enqueue(card1)))
          }
          else {
            val recDeck1 = newDeck1.take(card1)
            val recDeck2 = newDeck2.take(card2)
            playWinner((recDeck1, recDeck2)) match {
              case Left(_) =>
                Some((newDeck1.enqueue(card1).enqueue(card2), newDeck2))
              case Right(_) =>
                Some((newDeck1, newDeck2.enqueue(card2).enqueue(card1)))
            }
          }
        case (_, _) =>
          None
      }
    }
  }


  def parseDeck(s: String): Deck = s.linesIterator.drop(1).map(_.toInt).to(Queue) // TODO: Iterator tail

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
