package eu.sim642.adventofcode2023

object Day4 {

  case class Card(winning: Set[Int], have: Set[Int]) {
    def points: Int = {
      val haveWinning = (winning intersect have).size
      if (haveWinning == 0)
        0
      else
        1 << (haveWinning - 1)
    }
  }

  def sumPoints(cards: Seq[Card]): Int = cards.map(_.points).sum


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
  }
}
