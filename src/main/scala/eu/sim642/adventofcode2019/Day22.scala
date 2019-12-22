package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.NumberTheory
import eu.sim642.adventofcodelib.SeqImplicits._
import eu.sim642.adventofcodelib.IntegralImplicits._

object Day22 {

  type Deck = Seq[Int]

  sealed trait Technique {
    def apply(deck: Deck): Deck
    def applyPos(i: Int, size: Int): Int
  }

  case object DealIntoNewStack extends Technique {
    override def apply(deck: Deck): Deck = deck.reverse

    override def applyPos(i: Int, size: Int): Int = size - 1 - i
  }

  case class Cut(n: Int) extends Technique {
    override def apply(deck: Deck): Deck = deck.rotateLeft(n)

    override def applyPos(i: Int, size: Int): Int = (i - n) %+ size
  }

  case class DealWithIncrement(n: Int) extends Technique {
    override def apply(deck: Deck): Deck = {
      // TODO move to NumberTheory
      def modInv(a: Int, m: Int): Int = NumberTheory.bezoutCoefs(a, m)._1 %+ m

      val nInv = modInv(n, deck.size)

      Seq.tabulate(deck.size)(i => deck((nInv * i) % deck.size))
    }

    override def applyPos(i: Int, size: Int): Int = (n * i) %+ size
  }

  def shuffleFactoryOrder(techniques: Seq[Technique], n: Int): Deck = {
    techniques.foldLeft(Seq.range(0, n))((deck, technique) => technique(deck))
  }

  def shuffleFactoryOrderPosition(techniques: Seq[Technique], n: Int = 10007, card: Int = 2019): Int = {
    //shuffleFactoryOrder(techniques, n).indexOf(card)
    techniques.foldLeft(card)((i, technique) => technique.applyPos(i, n))
  }

  private val cutRegex = """cut (-?\d+)""".r
  private val dealWithIncrementRegex = """deal with increment (-?\d+)""".r // TODO: - disallowed?

  def parseTechnique(s: String): Technique = s match {
    case "deal into new stack" => DealIntoNewStack
    case cutRegex(n) => Cut(n.toInt)
    case dealWithIncrementRegex(n) => DealWithIncrement(n.toInt)
  }

  def parseTechniques(input: String): Seq[Technique] = input.linesIterator.map(parseTechnique).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(shuffleFactoryOrderPosition(parseTechniques(input)))
  }
}
