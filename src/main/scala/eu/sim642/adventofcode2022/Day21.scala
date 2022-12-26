package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.IntegralImplicits.*
import eu.sim642.adventofcodelib.{OrderedSearch, Rational}
import eu.sim642.adventofcodelib.Rational._

import scala.collection.mutable
import scala.math.Numeric
import scala.math.Numeric.{BigDecimalAsIfIntegral, LongIsIntegral}
import scala.math.Ordering.{BigDecimalOrdering, LongOrdering}

object Day21 {

  enum Op {
    case Add
    case Sub
    case Mul
    case Div
  }

  enum Job {
    case Number(value: Long)
    case Operation(lhs: String, op: Op, rhs: String)
  }

  type Monkeys = Map[String, Job]

  trait PseudoIntegral[A] extends Integral[A] {
    def fromLong(x: Long): A // need Long for BinarySearchPart2Solution
  }

  given PseudoIntegral[Long] = new PseudoIntegral[Long] with LongIsIntegral with LongOrdering {
    override def fromLong(x: Long): Long = x
  }

  given PseudoIntegral[BigDecimal] = new PseudoIntegral[BigDecimal] with BigDecimalAsIfIntegral with BigDecimalOrdering {
    override def fromLong(x: Long): BigDecimal = x

    override def quot(x: BigDecimal, y: BigDecimal): BigDecimal = x / y // pseudo-quot, fractional result
  }

  given [A](using aPseudoIntegral: PseudoIntegral[A]): PseudoIntegral[Rational[A]] = new PseudoIntegral[Rational[A]] with RationalNumeric[A] with RationalOrdering[A] {
    override def fromLong(x: Long): Rational[A] = Rational(aPseudoIntegral.fromLong(x))

    override def quot(x: Rational[A], y: Rational[A]): Rational[A] = x / y // pseudo-quot, fractional result

    override def rem(x: Rational[A], y: Rational[A]): Rational[A] = ???
  }

  def makeEvalName[A](monkeys: Monkeys)(using aPseudoIntegral: PseudoIntegral[A]): String => Option[A] = {
    val memo = mutable.Map.empty[String, Option[A]]

    def evalName(name: String): Option[A] =
      memo.getOrElseUpdate(name, monkeys.get(name).flatMap(evalJob))

    import Integral.Implicits._

    def evalJob(job: Job): Option[A] = job match {
      case Job.Number(value) => Some(aPseudoIntegral.fromLong(value))
      case Job.Operation(lhs, op, rhs) =>
        for {
          left <- evalName(lhs)
          right <- evalName(rhs)
        } yield op match {
          case Op.Add => left + right
          case Op.Sub => left - right
          case Op.Mul => left * right
          case Op.Div => left / right
        }
    }

    evalName
  }

  private val root = "root"

  def evalRoot(monkeys: Monkeys): Long = makeEvalName[Long](monkeys)(root).get

  private val humn = "humn"

  def makeHumnMonkeys(monkeys: Monkeys): Monkeys = {
    monkeys
      .updatedWith(root)({
        // change root to subtraction, so can solve for root == 0
        case Some(Job.Operation(lhs, _, rhs)) => Some(Job.Operation(lhs, Op.Sub, rhs))
      })
      .removed(humn)
  }

  trait Part2Solution {
    def findHumn(monkeys: Monkeys): Long
  }

  /**
   * Solution, which evaluates inversed top-down from target value to humn.
   * Assumes single occurrence of humn in expression.
   */
  object InvertPart2Solution extends Part2Solution {

    override def findHumn(monkeys: Monkeys): Long = {
      val humnMonkeys = makeHumnMonkeys(monkeys)
      val evalName = makeEvalName[Long](humnMonkeys)

      def invertName(name: String, target: Long): Option[Long] = {
        if (name == humn)
          Some(target)
        else
          invertJob(humnMonkeys(name), target)
      }

      def invertJob(job: Job, target: Long): Option[Long] = job match {
        case Job.Number(_) => None
        case Job.Operation(lhs, op, rhs) =>
          // left op right == target
          (evalName(lhs), evalName(rhs)) match {
            case (Some(left), None) =>
              val right = op match {
                case Op.Add => target - left
                case Op.Sub => left - target
                case Op.Mul => target / left
                case Op.Div => left / target
              }
              invertName(rhs, right)
            case (None, Some(right)) =>
              val left = op match {
                case Op.Add => target - right
                case Op.Sub => target + right
                case Op.Mul => target / right
                case Op.Div => target * right
              }
              invertName(lhs, left)
            case (_, _) => throw new IllegalArgumentException("job does not contain " + humn)
          }
      }

      invertName(root, 0).get
    }
  }

  /**
   * Solution, which uses binary search to find humn.
   * Assumes expression is monotonic w.r.t. humn.
   */
  object BinarySearchPart2Solution extends Part2Solution {

    override def findHumn(monkeys: Monkeys): Long = {
      val humnMonkeys = makeHumnMonkeys(monkeys)

      def f(humnValue: Long): Rational[Long] = {
        val humnMonkeys2 = humnMonkeys + (humn -> Job.Number(humnValue))
        makeEvalName[Rational[Long]](humnMonkeys2)(root).get
      }

      // OrderedSearch requires monotonic, negate if anti-monotonic
      val g = if (f(0) < f(1)) f else (x: Long) => -f(x)
      OrderedSearch.exponentialBinaryLower[Long, Rational[Long]](g, 0)(Rational(0L))
    }
  }

  /**
   * Solution, which solves linear equation to find humn.
   * Assumes expression is linear w.r.t. humn.
   */
  object LinearPart2Solution extends Part2Solution {

    override def findHumn(monkeys: Monkeys): Long = {
      val humnMonkeys = makeHumnMonkeys(monkeys)

      def f(humnValue: Long): Rational[Long] = {
        val humnMonkeys2 = humnMonkeys + (humn -> Job.Number(humnValue))
        makeEvalName[Rational[Long]](humnMonkeys2)(root).get
      }

      val y0 = f(0)
      val y1 = f(1)
      val dy = y1 - y0
      val x0 = y0 / -dy
      x0.toLong
    }
  }

  /**
   * Solution, which finds the derivative w.r.t. humn to get the slope
   * and then solves linear equation to find humn.
   * Assumes expression is linear w.r.t. humn.
   */
  object DerivativePart2Solution extends Part2Solution {

    override def findHumn(monkeys: Monkeys): Long = {
      val humnMonkeys = makeHumnMonkeys(monkeys) + (humn -> Job.Number(0)) // evaluate at 0
      val evalName = makeEvalName[Rational[Long]](humnMonkeys).andThen(_.get) // always get, because all variables set

      def deriveName(name: String): Rational[Long] = {
        if (name == humn)
          1
        else
          deriveJob(humnMonkeys(name))
      }

      def deriveJob(job: Job): Rational[Long] = job match {
        case Job.Number(_) => 0
        case Job.Operation(lhs, op, rhs) =>
          op match {
            case Op.Add => deriveName(lhs) + deriveName(rhs)
            case Op.Sub => deriveName(lhs) - deriveName(rhs)
            case Op.Mul =>
              deriveName(lhs) * evalName(rhs) + evalName(lhs) * deriveName(rhs)
            case Op.Div =>
              (deriveName(lhs) * evalName(rhs) - evalName(lhs) * deriveName(rhs)) / (evalName(rhs) * evalName(rhs))
          }
      }

      val y0 = evalName(root)
      val dy0 = deriveName(root) // derivative at 0
      val x0 = y0 / -dy0
      x0.toLong
    }
  }


  def parseMonkey(s: String): (String, Job) = s match {
    case s"$name: $lhs $opStr $rhs" =>
      val op = opStr match {
        case "+" => Op.Add
        case "-" => Op.Sub
        case "*" => Op.Mul
        case "/" => Op.Div
      }
      name -> Job.Operation(lhs, op, rhs)

    case s"$name: $value" => name -> Job.Number(value.toInt)
  }

  def parseMonkeys(input: String): Monkeys = input.linesIterator.map(parseMonkey).toMap

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import InvertPart2Solution._

    println(evalRoot(parseMonkeys(input)))
    println(findHumn(parseMonkeys(input)))
  }
}
