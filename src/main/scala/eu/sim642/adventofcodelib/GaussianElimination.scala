package eu.sim642.adventofcodelib

import scala.collection.mutable
import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.reflect.ClassTag

object GaussianElimination {

  case class Solution[A: Integral](dependentVars: Seq[Int], dependentGenerator: Seq[A],
                                   freeVars: Seq[Int], freeGenerators: Seq[Seq[A]],
                                   const: Seq[A]) {
    private lazy val freeGeneratorsTransposed = { // transpose of empty generators needs right length for lazyZip to work below
      if (freeGenerators.isEmpty)
        const.map(_ => Nil)
      else
        freeGenerators.transpose
    }

    require(dependentGenerator.size == const.size)
    require(dependentGenerator.size == freeGeneratorsTransposed.size)

    def evaluate(freeVals: Seq[A]): Seq[A] = {
      (const lazyZip freeGeneratorsTransposed lazyZip dependentGenerator).map((v, fgt, mainVar) => {
        val r = v - (fgt lazyZip freeVals).map(_ * _).sum
        if (r % mainVar == 0)
          r / mainVar
        else
          -summon[Integral[A]].one // TODO: Option
      })
    }
  }

  def solve[A: ClassTag](initialA: Seq[Seq[A]], initialb: Seq[A])(using aIntegral: Integral[A]): Solution[A] = {
    val rows = initialA zip initialb // TODO: lazyZip
    val m: mutable.ArraySeq[mutable.ArraySeq[A]] = rows.map((a, b) => (a :+ b).to(mutable.ArraySeq)).to(mutable.ArraySeq)
    val n = initialA.head.size

    def swapRows(y1: Int, y2: Int): Unit = {
      val row1 = m(y1)
      m(y1) = m(y2)
      m(y2) = row1
    }

    def simplifyRow(y: Int): Unit = {
      val factor = NumberTheory.gcd(m(y).toSeq) // TODO: avoid conversion
      if (factor.abs > summon[Integral[A]].one) {
        for (x <- 0 until (n + 1))
          m(y)(x) /= factor
      }
    }

    def reduceRow(x: Int, y1: Int, y2: Int): Unit = {
      val c2 = m(y2)(x)
      if (c2 != 0) {
        val c1 = m(y1)(x)
        val (factor1, factor2) = NumberTheory.extendedGcd(c1, c2)._3
        for (x2 <- 0 until x) // must start from 0 because we're now multiplying entire row y2
          m(y2)(x2) = factor2 * m(y2)(x2)
        for (x2 <- x until (n + 1))
          m(y2)(x2) = factor2 * m(y2)(x2) + factor1 * m(y1)(x2)
        //simplifyRow(y2) // TODO: helps?
      }
    }

    // forward elimination
    var y = 0
    for (x <- 0 until n) {
      (y until m.size).find(m(_)(x) != 0) match {
        case None => // move to next x
        case Some(y2) =>
          swapRows(y, y2)
          for (y3 <- (y + 1) until m.size)
            reduceRow(x, y, y3)
          y += 1
      }
    }

    // check consistency
    for (y2 <- y until m.size)
      assert(m(y2).last == 0) // TODO: return Option

    // backward elimination
    val dependentVars = mutable.ArrayBuffer.empty[Int]
    val freeVars = mutable.ArrayBuffer.empty[Int]
    y = 0
    for (x <- 0 until n) {
      if (y >= m.size || m(y)(x) == 0)
        freeVars += x
      else {
        dependentVars += x
        for (y2 <- 0 until y)
          reduceRow(x, y, y2)
        y += 1
      }
    }

    Solution(
      dependentVars = dependentVars.toSeq,
      dependentGenerator = (dependentVars lazyZip m).view.map((v, row) => row(v)).toSeq,
      freeVars = freeVars.toSeq,
      freeGenerators = freeVars.view.map(x => m.view.take(dependentVars.size).map(_(x)).toSeq).toSeq,
      const = m.view.take(dependentVars.size).map(_.last).toSeq
    )
  }
}
