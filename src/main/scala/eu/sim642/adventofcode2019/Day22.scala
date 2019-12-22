package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.NumberTheory
import eu.sim642.adventofcodelib.SeqImplicits._
import eu.sim642.adventofcodelib.IntegralImplicits._
import Integral.Implicits._

object Day22 {

  type Deck = Seq[Int]

  sealed trait Technique {
    def apply(deck: Deck): Deck
    def applyPos(i: Long, size: Long): Long
    def applyPosReverse(i: Long, size: Long): Long
  }

  case object DealIntoNewStack extends Technique {
    override def apply(deck: Deck): Deck = deck.reverse

    override def applyPos(i: Long, size: Long): Long = size - 1 - i

    override def applyPosReverse(i: Long, size: Long): Long = size - 1 - i
  }

  case class Cut(n: Int) extends Technique {
    override def apply(deck: Deck): Deck = deck.rotateLeft(n)

    override def applyPos(i: Long, size: Long): Long = (i - n) %+ size

    override def applyPosReverse(i: Long, size: Long): Long = (i + n) %+ size
  }

  def modInv(a: Long, m: Long): Long = NumberTheory.bezoutCoefs(a, m)._1 %+ m

  case class DealWithIncrement(n: Int) extends Technique {
    // TODO move to NumberTheory

    override def apply(deck: Deck): Deck = {
      val nInv = modInv(n, deck.size)
      Seq.tabulate(deck.size)(i => deck(((nInv * i) % deck.size).toInt))
    }

    override def applyPos(i: Long, size: Long): Long = (n * i) %+ size

    override def applyPosReverse(i: Long, size: Long): Long = {
      val nInv = modInv(n, size)
      //(nInv * i) %+ size
      ((BigInt(nInv) * i) %+ size).toLong
    }
  }

  def shuffleFactoryOrder(techniques: Seq[Technique], n: Int): Deck = {
    techniques.foldLeft(Seq.range(0, n))((deck, technique) => technique(deck))
  }

  def shuffleFactoryOrderPosition(techniques: Seq[Technique], n: Long = 10007, card: Long = 2019): Long = {
    //shuffleFactoryOrder(techniques, n).indexOf(card)
    techniques.foldLeft(card)((i, technique) => technique.applyPos(i, n))
  }

  case class Modular(m: Long) {
    case class Linear(a: Long, b: Long) {
      // ax + b

      private def longMul(a: Long, b: Long): Long = {
        ((BigInt(a) * b) %+ m).toLong
      }

      def compose(that: Linear): Linear = {
        val Linear(c, d) = that // cx + d
        // a(cx + d) + b = (ac)x + (ad + b)
        Linear(longMul(a, c), (longMul(a, d) + b) %+ m)
      }

      def apply(i: Long): Long = (longMul(a, i) + b) %+ m

      // copied from IntegralImplicits
      // TODO: optimize using tailrec or loop
      def pow(n: Long): Linear = {
        if (n == 0)
          Linear.identity
        else {
          val (q, r) = n /% 2
          val half = pow(q)
          val halfSquare = half compose half
          if (r == 0)
            halfSquare
          else
            this compose halfSquare
        }
      }

      def inverse: Linear = {
        // ax + b = y (mod size)
        // x = aInv * (y - b) (mod size)
        // x = aInv * y - aInv * b (mod size)
        // aInv * y - aInv * b = x
        val aInv = modInv(a, m)
        Linear(aInv, (-longMul(aInv, b)) %+ m)
      }
    }

    object Linear {
      val identity: Linear = Linear(1, 0)
    }

  }

  def shuffleFactoryOrderPositionReverse(techniques: Seq[Technique], size: Long = 119315717514047L, i: Long = 2020): Long = {

    // TODO: clean this mess up

    val modular = Modular(size)
    import modular._

    def toLinear(technique: Technique): Linear = technique match {
      case DealIntoNewStack => Linear(-1, size - 1)
      case Cut(n) => Linear(1, (-n.toLong) %+ size)
      case DealWithIncrement(n) => Linear(n, 0)
    }

    val linear = techniques.map(toLinear).reduce((a, b) => b.compose(a))

    val linear2 = linear.pow(101741582076661L)
    linear2.inverse(i)
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
    println(shuffleFactoryOrderPositionReverse(parseTechniques(input)))

    // 40340147547378 - too low
  }
}
