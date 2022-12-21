package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.IntegralImplicits.*
import eu.sim642.adventofcodelib.OrderedSearch

import scala.collection.mutable
import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Numeric.{BigDecimalIsFractional, DoubleIsFractional, LongIsIntegral}

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

  trait Divisble[A] extends Numeric[A] {
    def div(x: A, y: A): A
    def fromLong(x: Long): A
  }

  given Divisble[Long] = new Divisble[Long] with LongIsIntegral with Ordering.LongOrdering {
    override def div(x: Long, y: Long): Long = x / y

    override def fromLong(x: Long): Long = x
  }
  given Divisble[Double] = new Divisble[Double] with DoubleIsFractional with Ordering.Double.TotalOrdering {
    override def fromLong(x: Long): Double = x.toDouble
  }
  given Divisble[BigDecimal] = new Divisble[BigDecimal] with BigDecimalIsFractional with Ordering.BigDecimalOrdering {
    override def fromLong(x: Long): BigDecimal = BigDecimal(x)
  }

  def makeEvalName[A](monkeys: Monkeys)(using aDivisble: Divisble[A]): String => Option[A] = {
    val memo = mutable.Map.empty[String, Option[A]]

    def evalName(name: String): Option[A] =
      memo.getOrElseUpdate(name, monkeys.get(name).flatMap(evalJob))

    def evalJob(job: Job): Option[A] = job match {
      case Job.Number(value) => Some(aDivisble.fromLong(value))
      case Job.Operation(lhs, op, rhs) =>
        for {
          left <- evalName(lhs)
          right <- evalName(rhs)
        } yield op match {
          case Op.Add => left + right
          case Op.Sub => left - right
          case Op.Mul => left * right
          case Op.Div => aDivisble.div(left, right)
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

      def f(humnValue: Long): BigDecimal = {
        val humnMonkeys2 = humnMonkeys + (humn -> Job.Number(humnValue))
        makeEvalName[BigDecimal](humnMonkeys2)(root).get
      }

      // OrderedSearch requires monotonic, negate if anti-monotonic
      val g = if (f(0) < f(1)) f else (x: Long) => -f(x)
      OrderedSearch.exponentialBinaryLower[Long, BigDecimal](g, 0)(0L)
    }
  }

  /**
   * Solution, which solves linear equation to find humn.
   * Assumes expression is linear w.r.t. humn.
   */
  object LinearPart2Solution extends Part2Solution {

    override def findHumn(monkeys: Monkeys): Long = {
      val humnMonkeys = makeHumnMonkeys(monkeys)

      def f(humnValue: Long): BigDecimal = {
        val humnMonkeys2 = humnMonkeys + (humn -> Job.Number(humnValue))
        makeEvalName[BigDecimal](humnMonkeys2)(root).get
      }

      val y0 = f(0)
      val y1 = f(1)
      val dy = y1 - y0
      val x0 = y0 / -dy
      x0.setScale(0, BigDecimal.RoundingMode.HALF_UP).longValue
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
