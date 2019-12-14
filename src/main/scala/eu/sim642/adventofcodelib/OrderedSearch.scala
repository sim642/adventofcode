package eu.sim642.adventofcodelib

import Ordering.Implicits._
import Integral.Implicits._
import scala.annotation.tailrec

object OrderedSearch {

  /**
    * Finds the smallest (first) element, which is at least `x` (lower bound).
    * If image of `f` contains `x`, then it is the leftmost `x`.
    *
    * @param min inclusive
    * @param max exclusive
    *
    * @see [[https://en.wikipedia.org/wiki/Binary_search_algorithm#Procedure_for_finding_the_leftmost_element]]
    * @see [[https://en.cppreference.com/w/cpp/algorithm/lower_bound]]
    */
  def binaryLower[A, B](f: A => B, min: A, max: A)(x: B)(implicit aIntegral: Integral[A], bOrdering: Ordering[B]): A = {
    val aOne = aIntegral.one
    val aTwo = aIntegral.fromInt(2)

    @tailrec
    def helper(lo: A, hi: A): A = {
      if (lo >= hi)
        lo // lower specific
      else {
        val mid = (lo + hi) / aTwo
        if (f(mid) >= x) // lower specific
          helper(lo, mid)
        else
          helper(mid + aOne, hi)
      }
    }

    helper(min, max)
  }

  /**
    * Finds the largest (last) element, which is at most `x` (upper bound).
    * If image of `f` contains `x`, then it is the rightmost `x`.
    *
    * @param min inclusive
    * @param max exclusive
    *
    * @see [[https://en.wikipedia.org/wiki/Binary_search_algorithm#Procedure_for_finding_the_rightmost_element]]
    * @see [[https://en.cppreference.com/w/cpp/algorithm/upper_bound]] (offset by 1)
    */
  def binaryUpper[A, B](f: A => B, min: A, max: A)(x: B)(implicit aIntegral: Integral[A], bOrdering: Ordering[B]): A = {
    val aOne = aIntegral.one
    val aTwo = aIntegral.fromInt(2)

    @tailrec
    def helper(lo: A, hi: A): A = {
      if (lo >= hi)
        lo - aOne // upper specific
      else {
        val mid = (lo + hi) / aTwo
        if (f(mid) > x) // upper specific
          helper(lo, mid)
        else
          helper(mid + aOne, hi)
      }
    }

    helper(min, max)
  }
}
