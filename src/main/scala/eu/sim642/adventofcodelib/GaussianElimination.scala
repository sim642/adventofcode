package eu.sim642.adventofcodelib

import scala.collection.mutable
import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.reflect.ClassTag

object GaussianElimination {

  case class Solution[A: Integral](dependentVars: Seq[Int], dependentGenerator: Seq[A],
                                   freeVars: Seq[Int], freeGenerators: Seq[Seq[A]],
                                   const: Seq[A]) {
    def evaluate(freeVals: Seq[A]): Seq[A] = {
      (dependentGenerator lazyZip const).zipWithIndex.map({case ((mainVar, v), i) =>
        val r = v - (freeGenerators lazyZip freeVals).map((freeVar, freeVal) => freeVar(i) * freeVal).sum
        if (r % mainVar == 0)
          r / mainVar
        else
          -summon[Integral[A]].one // TODO: Option
      }).toList
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

    def multiplyRow(y: Int, factor: A): Unit = {
      for (x2 <- 0 until (n + 1))
        m(y)(x2) *= factor
    }

    def simplifyRow(y: Int): Unit = {
      val factor = NumberTheory.gcd(m(y).toSeq) // TODO: avoid conversion
      if (factor.abs > summon[Integral[A]].one) {
        for (x2 <- 0 until (n + 1))
          m(y)(x2) /= factor
      }
    }

    def reduceDown(x: Int, y1: Int, y2: Int): Unit = {
      val c2 = m(y2)(x)
      if (c2 != 0) {
        val c1 = m(y1)(x)
        val (_, _, (factor, factor2)) = NumberTheory.extendedGcd(c1, c2)
        for (x2 <- 0 until x) // must start from 0 because we're now multiplying entire row y2
          m(y2)(x2) = factor2 * m(y2)(x2)
        for (x2 <- x until (n + 1))
          m(y2)(x2) = factor2 * m(y2)(x2) + factor * m(y1)(x2)
        //simplifyRow(y2) // TODO: helps?
      }
    }

    var y = 0
    for (x <- 0 until n) {
      val y2opt = m.indices.find(y2 => y2 >= y && m(y2)(x) != 0)
      y2opt match {
        case None => // move to next x
        case Some(y2) =>
          swapRows(y, y2)
          for (y3 <- (y + 1) until m.size)
            reduceDown(x, y, y3)

          y += 1
      }
    }

    // check consistency
    for (y2 <- y until m.size)
      assert(m(y2).last == 0) // TODO: return Option

    val mainVars = mutable.ArrayBuffer.empty[Int]
    val freeVars = mutable.ArrayBuffer.empty[Int]
    y = 0
    for (x <- 0 until n) {
      if (y < m.size) { // TODO: break if y too big
        if (m(y)(x) == 0) {
          freeVars += x
          ()
        } // move to next x
        else {
          mainVars += x
          for (y3 <- 0 until y)
            reduceDown(x, y, y3)

          y += 1
        }
      }
      else
        freeVars += x // can't break if this is here
    }

    Solution(
      dependentVars = mainVars.toSeq,
      dependentGenerator = (mainVars lazyZip m).view.map((v, row) => row(v)).toSeq,
      freeVars = freeVars.toSeq,
      freeGenerators = freeVars.view.map(x => m.view.map(_(x)).toSeq).toSeq,
      const = m.view.map(_.last).toSeq
    )
  }
}
