package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.IntegralImplicits.*

import scala.collection.mutable

object Day21 {

  enum Op {
    case Add
    case Sub
    case Mul
    case Div
  }

  enum Job {
    case Number(value: Int)
    case Operation(lhs: String, op: Op, rhs: String)
  }

  type Monkeys = Map[String, Job]

  def makeEvalName(monkeys: Monkeys): String => Option[Long] = {
    val memo = mutable.Map.empty[String, Option[Long]]

    def evalName(name: String): Option[Long] =
      memo.getOrElseUpdate(name, monkeys.get(name).flatMap(evalJob))

    def evalJob(job: Job): Option[Long] = job match {
      case Job.Number(value) => Some(value)
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

  def evalRoot(monkeys: Monkeys): Long = makeEvalName(monkeys)(root).get

  private val humn = "humn"

  def makeHumnMonkeys(monkeys: Monkeys): Monkeys = {
    monkeys
      .updatedWith(root)({
        // change root to subtraction, so can solve for root == 0
        case Some(Job.Operation(lhs, _, rhs)) => Some(Job.Operation(lhs, Op.Sub, rhs))
      })
      .removed(humn)
  }

  def findHumn(monkeys: Monkeys): Long = {
    val humnMonkeys = makeHumnMonkeys(monkeys)
    val evalName = makeEvalName(humnMonkeys)

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
    println(evalRoot(parseMonkeys(input)))
    println(findHumn(parseMonkeys(input)))
  }
}
