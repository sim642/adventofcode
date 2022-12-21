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

  def evalRoot(monkeys: Monkeys): Long = {

    val memo = mutable.Map.empty[String, Long]

    def evalName(name: String): Long = memo.getOrElseUpdate(name, evalJob(monkeys(name)))

    def evalJob(job: Job): Long = job match {
      case Job.Number(value) => value
      case Job.Operation(lhs, op, rhs) =>
        val left = evalName(lhs)
        val right = evalName(rhs)
        op match {
          case Op.Add => left + right
          case Op.Sub => left - right
          case Op.Mul => left * right
          case Op.Div => left / right
        }
    }

    evalName("root")
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
  }
}
