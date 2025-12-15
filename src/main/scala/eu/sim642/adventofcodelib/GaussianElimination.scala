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
    val m = initialA.size
    val n = initialA.head.size
    require(initialb.sizeIs == m)

    val A: mutable.ArraySeq[mutable.ArraySeq[A]] = initialA.map(_.to(mutable.ArraySeq)).to(mutable.ArraySeq)
    val b: mutable.ArraySeq[A] = initialb.to(mutable.ArraySeq)

    def swapRows(y1: Int, y2: Int): Unit = {
      val A1 = A(y1)
      A(y1) = A(y2)
      A(y2) = A1
      val b1 = b(y1)
      b(y1) = b(y2)
      b(y2) = b1
    }

    def simplifyRow(y: Int): Unit = {
      val factor = NumberTheory.gcd(NumberTheory.gcd(A(y).toSeq), b(y)) // TODO: avoid conversion
      if (factor.abs > summon[Integral[A]].one) {
        A(y).mapInPlace(_ / factor)
        b(y) /= factor
      }
    }

    def reduceRow(x: Int, y1: Int, y2: Int): Unit = {
      val c2 = A(y2)(x)
      if (c2 != 0) {
        val c1 = A(y1)(x)
        val (factor1, factor2) = NumberTheory.extendedGcd(c1, c2)._3
        for (x2 <- 0 until x) // must start from 0 because we're now multiplying entire row y2
          A(y2)(x2) = factor2 * A(y2)(x2)
        for (x2 <- x until n)
          A(y2)(x2) = factor2 * A(y2)(x2) + factor1 * A(y1)(x2)
        b(y2) = factor2 * b(y2) + factor1 * b(y1)
        //simplifyRow(y2) // TODO: helps?
      }
    }

    // forward elimination
    var y = 0
    for (x <- 0 until n) {
      (y until m).find(A(_)(x) != 0) match {
        case None => // move to next x
        case Some(y2) =>
          swapRows(y, y2)
          for (y3 <- (y + 1) until m)
            reduceRow(x, y, y3)
          y += 1
      }
    }

    // check consistency
    for (y2 <- y until b.size)
      assert(b(y2) == 0) // TODO: return Option

    // backward elimination
    val dependentVars = mutable.ArrayBuffer.empty[Int]
    val freeVars = mutable.ArrayBuffer.empty[Int]
    y = 0
    for (x <- 0 until n) {
      if (y >= m || A(y)(x) == 0)
        freeVars += x
      else {
        dependentVars += x
        for (y2 <- 0 until y)
          reduceRow(x, y, y2)
        y += 1
      }
    }

    val Aview = A.view.take(dependentVars.size)
    Solution(
      dependentVars = dependentVars.toSeq,
      dependentGenerator = (A lazyZip dependentVars).map(_(_)).toSeq,
      freeVars = freeVars.toSeq,
      freeGenerators = freeVars.view.map(x => Aview.map(_(x)).toSeq).toSeq,
      const = b.view.take(dependentVars.size).toSeq
    )
  }
}
