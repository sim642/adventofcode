package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.NumberTheory
import eu.sim642.adventofcodelib.SeqImplicits._
import eu.sim642.adventofcodelib.IntegralImplicits._
import Integral.Implicits._

object Day22 {

  sealed trait Technique
  case object DealIntoNewStack extends Technique
  case class Cut(n: Int) extends Technique
  case class DealWithIncrement(n: Int) extends Technique

  type Techniques = Seq[Technique]


  trait Part1Solution {
    def shuffleFactoryOrderPosition(techniques: Techniques, size: Long = 10007, card: Long = 2019): Long
  }

  trait Part2Solution {
    def shuffleFactoryOrderPositionInverse(techniques: Techniques, size: Long = 119315717514047L, i: Long = 2020): Long

    val inverseRepeat = 101741582076661L
  }

  object NaivePart1Solution extends Part1Solution {
    type Deck = Vector[Long]

    def applyDeck(technique: Technique, deck: Deck): Deck = technique match {
      case DealIntoNewStack => deck.reverse
      case Cut(n) => deck.rotateLeft(n)
      case DealWithIncrement(n) =>
        val size = deck.size
        val nInv = NumberTheory.modInv(n, size)
        Vector.tabulate(size)(i => deck(((nInv * i) % size).toInt))
    }

    def shuffleFactoryOrder(techniques: Techniques, size: Long): Deck = {
      techniques.foldLeft(Vector.range(0, size))((deck, technique) => applyDeck(technique, deck))
    }

    override def shuffleFactoryOrderPosition(techniques: Techniques, size: Long, card: Long): Long = {
      shuffleFactoryOrder(techniques, size).indexOf(card)
    }
  }

  object PositionPart1Solution extends Part1Solution {
    def applyPosition(technique: Technique, i: Long, size: Long): Long = technique match {
      case DealIntoNewStack => size - 1 - i
      case Cut(n) => (i - n) %+ size
      case DealWithIncrement(n) => (n * i) %+ size
    }

    override def shuffleFactoryOrderPosition(techniques: Techniques, size: Long, card: Long): Long = {
      techniques.foldLeft(card)((i, technique) => applyPosition(technique, i, size))
    }
  }

  object LinearSolution extends Part1Solution with Part2Solution {
    case class Modular(m: Long) {
      case class Linear(a: Long, b: Long) {
        // ax + b

        private def safeMul(a: Long, b: Long): Long = {
          ((BigInt(a) * b) %+ m).toLong
        }

        def compose(that: Linear): Linear = {
          val Linear(c, d) = that // cx + d
          // a(cx + d) + b = (ac)x + (ad + b)
          Linear(safeMul(a, c), (safeMul(a, d) + b) %+ m)
        }

        def apply(i: Long): Long = (safeMul(a, i) + b) %+ m

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
          val aInv = NumberTheory.modInv(a, m)
          Linear(aInv, (-safeMul(aInv, b)) %+ m)
        }
      }

      object Linear {
        val identity: Linear = Linear(1, 0)
      }
    }

    def toLinear(technique: Technique, modular: Modular): modular.Linear = {
      import modular._
      val size = m

      technique match {
        case DealIntoNewStack => Linear(-1, size - 1)
        case Cut(n) => Linear(1, (-n.toLong) %+ size)
        case DealWithIncrement(n) => Linear(n, 0)
      }
    }

    def toLinear(techniques: Techniques, modular: Modular): modular.Linear = {
      techniques.map(toLinear(_, modular)).reduce((a, b) => b.compose(a))
    }

    override def shuffleFactoryOrderPosition(techniques: Techniques, size: Long, card: Long): Long = {
      val modular = Modular(size)
      val linear = toLinear(techniques, modular)
      linear(card)
    }

    override def shuffleFactoryOrderPositionInverse(techniques: Techniques, size: Long, i: Long): Long = {
      val modular = Modular(size)
      val linear = toLinear(techniques, modular)
      val linearRepeat = linear.pow(inverseRepeat)
      linearRepeat.inverse(i)
    }
  }

  private val cutRegex = """cut (-?\d+)""".r
  private val dealWithIncrementRegex = """deal with increment (-?\d+)""".r // TODO: - disallowed?

  def parseTechnique(s: String): Technique = s match {
    case "deal into new stack" => DealIntoNewStack
    case cutRegex(n) => Cut(n.toInt)
    case dealWithIncrementRegex(n) => DealWithIncrement(n.toInt)
  }

  def parseTechniques(input: String): Techniques = input.linesIterator.map(parseTechnique).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import LinearSolution._

    println(shuffleFactoryOrderPosition(parseTechniques(input)))
    println(shuffleFactoryOrderPositionInverse(parseTechniques(input)))

    // 40340147547378 - too low
  }
}
