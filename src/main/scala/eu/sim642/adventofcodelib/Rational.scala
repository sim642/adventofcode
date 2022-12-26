package eu.sim642.adventofcodelib

import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.Ordering.Implicits.infixOrderingOps

class Rational[A](numerator: A, denumerator: A)(using aIntegral: Integral[A]) extends Ordered[Rational[A]] {
  val (n, d) = {
    // https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Simplification_of_fractions
    val (s, t) = NumberTheory.extendedGcd(numerator, denumerator)._3
    require(s != aIntegral.zero, "division by zero")
    if (s >= aIntegral.zero)
      (-t, s)
    else
      (t, -s)
  }

  def +(that: Rational[A]): Rational[A] =
    Rational(n * that.d + that.n * d, d * that.d)

  def -(that: Rational[A]): Rational[A] =
    Rational(n * that.d - that.n * d, d * that.d)

  def *(that: Rational[A]): Rational[A] =
    Rational(n * that.n, d * that.d)

  def /(that: Rational[A]): Rational[A] =
    Rational(n * that.d, d * that.n)

  def unary_- : Rational[A] = Rational(-n, d)

  override def compare(that: Rational[A]): Int = {
    aIntegral.compare(n * that.d, that.n * d)
  }

  override def toString: String = {
    if (d == aIntegral.one)
      s"$n"
    else
      s"$n/$d"
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Rational[A]]

  override def equals(other: Any): Boolean = other match {
    case that: Rational[A] =>
      (that canEqual this) &&
        n == that.n &&
        d == that.d
    case _ => false
  }

  override def hashCode(): Int = 31 * n.hashCode() + d.hashCode()

  def toLong: Long = (n / d).toLong
}

object Rational {

  def apply[A: Integral](numerator: A, denumerator: A): Rational[A] =
    new Rational(numerator, denumerator)

  def apply[A](integer: A)(using aIntegral: Integral[A]): Rational[A] =
    Rational(integer, aIntegral.one)

  trait RationalOrdering[A] extends Ordering[Rational[A]] {
    override def compare(x: Rational[A], y: Rational[A]): Int = x.compare(y)
  }

  trait RationalNumeric[A: Integral] extends Numeric[Rational[A]] {
    override def plus(x: Rational[A], y: Rational[A]): Rational[A] = x + y

    override def minus(x: Rational[A], y: Rational[A]): Rational[A] = x - y

    override def times(x: Rational[A], y: Rational[A]): Rational[A] = x * y

    override def negate(x: Rational[A]): Rational[A] = -x

    override def fromInt(x: Int): Rational[A] = Rational(summon[Integral[A]].fromInt(x))
    //override def fromInt(x: Int): Rational[A] = ???

    override def parseString(str: String): Option[Rational[A]] = ???

    override def toInt(x: Rational[A]): Int = ???

    override def toLong(x: Rational[A]): Long = ???

    override def toFloat(x: Rational[A]): Float = ???

    override def toDouble(x: Rational[A]): Double = ???
  }

  given [A](using aIntegral: Integral[A]): Conversion[Int, Rational[A]] = new Conversion[Int, Rational[A]] {
    override def apply(x: Int): Rational[A] = Rational(aIntegral.fromInt(x))
  }
}
