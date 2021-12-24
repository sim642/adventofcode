package eu.sim642.adventofcode2021

import scala.annotation.tailrec
import scala.collection.mutable

object Day24 {

  case class Step(a: Int, b: Int, d: Int)

  def solve(steps: Seq[Step]): Seq[Range] = {

    @tailrec
    def helper(steps: List[Step], i: Int, stack: List[(Int, Int)], acc: List[(Int, Int, Int)]): List[(Int, Int, Int)] = steps match {
      case Nil =>
        acc
      case Step(a, b, d) :: newSteps =>
        stack match {
          case (popB, popI) :: newStack =>
            if ((0 to 9).contains((popB + a).abs)) {
              helper(newSteps, i + 1, newStack, (popB + a, popI, i) :: acc)
            }
            else {
              helper(newSteps, i + 1, (b, i) :: stack, acc)
            }
          case Nil =>
            helper(newSteps, i + 1, (b, i) :: stack, acc)
        }
    }

    val constraints = helper(steps.toList, 0, Nil, Nil)
    val digits = mutable.IndexedSeq.fill(steps.size)(1 to 9)
    for ((diff, i, j) <- constraints) {
      if (diff > 0) {
        digits(i) = digits(i).min to (9 - diff)
        digits(j) = (1 + diff) to digits(j).max
      }
      else if (diff < 0) {
        digits(i) = (1 - diff) to digits(i).max
        digits(j) = digits(j).min to (9 + diff)
      }
    }

    digits.toSeq
  }

  def maxModelNumber(steps: Seq[Step]): String = {
    solve(steps).map(_.max).mkString
  }

  def minModelNumber(steps: Seq[Step]): String = {
    solve(steps).map(_.min).mkString
  }


  private val stepRegex =
    """inp w
      |mul x 0
      |add x z
      |mod x 26
      |div z (1|26)
      |add x (-?\d+)
      |eql x w
      |eql x 0
      |mul y 0
      |add y 25
      |mul y x
      |add y 1
      |mul z y
      |mul y 0
      |add y w
      |add y (\d+)
      |mul y x
      |add z y""".stripMargin.r

  def parseSteps(input: String): Seq[Step] = {
    stepRegex.findAllMatchIn(input).map(m =>
      Step(m.group(2).toInt, m.group(3).toInt, m.group(1).toInt)
    ).toSeq
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(maxModelNumber(parseSteps(input)))
    println(minModelNumber(parseSteps(input)))
  }
}
