package eu.sim642.adventofcode2020

import scala.collection.immutable.Queue
import eu.sim642.adventofcodelib.LazyListImplicits._

object Day22 {

  type Deck = Queue[Int]
  type Decks = (Deck, Deck)

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

  def play(decks: Decks): LazyList[Decks] = LazyList.unfold0(decks)(playRound)

  def winningScore(decks: Decks): Int = {
    val (deck1, deck2) = play(decks).last
    val winningDeck = if (deck1.isEmpty) deck2 else deck1
    winningDeck
      .reverseIterator
      .zipWithIndex
      .map({ case (card, i) => card * (i + 1) })
      .sum
  }


  def parseDeck(s: String): Deck = s.linesIterator.drop(1).map(_.toInt).to(Queue) // TODO: Iterator tail

  def parseDecks(input: String): Decks = {
    val Seq(s1, s2) = input.split("\n\n").toSeq
    (parseDeck(s1), parseDeck(s2))
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(winningScore(parseDecks(input)))
  }
}
