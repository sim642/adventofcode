package eu.sim642.adventofcode2021

import scala.annotation.tailrec
import scala.collection.mutable

object Day24 {

  case class Step(a: Int, b: Int, d: Int)

  /**
   * Digit constraint of the form: digits[j] = digits[i] + diff.
   */
  case class Constraint(diff: Int, i: Int, j: Int) {
    /**
     * Normalize constraint, such that diff >= 0.
     */
    def normalized: Constraint = {
      if (diff >= 0)
        this
      else
        Constraint(-diff, j, i)
    }
  }

  def generateConstraints(steps: Seq[Step]): Seq[Constraint] = {

    case class HalfConstraint(diff: Int, i: Int)

    @tailrec
    def helper(steps: List[Step], i: Int, stack: List[HalfConstraint], acc: List[Constraint]): List[Constraint] = steps match {
      case Nil =>
        acc
      case Step(a, b, d) :: newSteps =>
        stack match {
          case HalfConstraint(popB, popI) :: newStack if (0 to 9).contains((popB + a).abs) =>
            assert(d == 26)
            assert(a <= 0) // allow 0 for emilyskidsister input
            helper(newSteps, i + 1, newStack, Constraint(popB + a, popI, i) :: acc)
          case _ =>
            assert(d == 1)
            assert(a > 0)
            helper(newSteps, i + 1, HalfConstraint(b, i) :: stack, acc)
        }
    }

    helper(steps.toList, 0, Nil, Nil)
  }

  def solveConstraints(n: Int, constraints: Seq[Constraint]): Seq[Range.Inclusive] = {
    val digits = mutable.IndexedSeq.fill(n)(1 to 9) // easier with mutable
    // assume single-pass solvability (every digit only in at most one constraint)
    for (Constraint(diff, i, j) <- constraints.map(_.normalized)) {
      // assume normalized to reduce duplication
      digits(i) = digits(i).min to (9 - diff)
      digits(j) = (1 + diff) to digits(j).max
    }
    digits.toSeq
  }

  def solve(steps: Seq[Step]): Seq[Range] = {
    val constraints = generateConstraints(steps)
    solveConstraints(steps.size, constraints)
  }

  def maxModelNumber(steps: Seq[Step]): String = {
    solve(steps).map(_.max).mkString
  }

  def minModelNumber(steps: Seq[Step]): String = {
    solve(steps).map(_.min).mkString
  }


  private lazy val stepRegex = io.Source.fromInputStream(getClass.getResourceAsStream("day24/step_regex.txt")).mkString.trim.r

  def parseSteps(input: String): Seq[Step] = {
    stepRegex.findAllMatchIn(input).map(m =>
      Step(m.group("a").toInt, m.group("b").toInt, m.group("d").toInt)
    ).toSeq
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(maxModelNumber(parseSteps(input)))
    println(minModelNumber(parseSteps(input)))
  }
}
