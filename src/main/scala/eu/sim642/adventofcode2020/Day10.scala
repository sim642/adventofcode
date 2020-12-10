package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.IteratorImplicits._

import scala.annotation.tailrec
import scala.language.implicitConversions

object Day10 {

  def differencesProduct(jolts: Seq[Int]): Int = {
    val initialJolt = 0
    val builtinJolt = jolts.max + 3
    val allJolts = initialJolt +: builtinJolt +: jolts
    val diffs = allJolts.sorted.iterator.zipWithTail.map({ case (a, b) => b - a }).toSeq
    diffs.count(_ == 1) * diffs.count(_ == 3)
  }

  sealed trait Part2Solution {
    def countArrangements(jolts: Seq[Int]): Long
  }

  object DynamicProgrammingPart2Solution extends Part2Solution {
    override def countArrangements(jolts: Seq[Int]): Long = {
      val builtinJolt = jolts.max + 3
      val allJolts = builtinJolt +: jolts

      // a_0 = 1
      // a_1 = J(1) * a_0
      // a_2 = J(2) * a_0 + J(2) * a_1 = J(2) * (a_0 + a_1)
      // a_3 = J(3) * (a_0 + a_1 + a_2)
      // a_4 = J(4) * (a_1 + a_2 + a_3)
      // ...
      // a_n = J(n) * (a_(n-3) + a_(n-2) + a_(n-1))

      @tailrec
      def helper(jolts: List[Int], prevs: Map[Int, Long]): Long = jolts match {
        case Nil => prevs(builtinJolt)
        case jolt :: newJolts =>
          val joltValue = prevs(jolt - 3) + prevs(jolt - 2) + prevs(jolt - 1)
          val newPrevs = prevs + (jolt -> joltValue)
          helper(newJolts, newPrevs)
      }

      helper(allJolts.sorted.toList, Map(0 -> 1L).withDefaultValue(0))
    }
  }

  object ListKnotTyingPart2Solution extends Part2Solution {
    override def countArrangements(jolts: Seq[Int]): Long = {
      val builtinJolt = jolts.max + 3
      val allJolts = jolts.toSet + builtinJolt

      def J(jolt: Int): Int = if (allJolts.contains(jolt)) 1 else 0

      val arrangements0 = 1L
      val arrangements1 = J(1) * arrangements0
      val arrangements2 = J(2) * (arrangements0 + arrangements1)
      lazy val arrangements: LazyList[Long] = arrangements0 #:: arrangements1 #:: arrangements2 #:: arrangements.lazyZip(arrangements.tail).lazyZip(arrangements.drop(2)).zipWithIndex.map({ case ((prev3, prev2, prev1), i) =>
        J(i + 3) * (prev3 + prev2 + prev1)
      }).to(LazyList)

      arrangements(builtinJolt)
    }
  }

  object MapKnotTyingPart2Solution extends Part2Solution {
    override def countArrangements(jolts: Seq[Int]): Long = {
      val builtinJolt = jolts.max + 3
      val allJolts = builtinJolt +: jolts

      // TODO: move lazy things to library
      class LazyCell[+A](value: => A) {
        lazy val get: A = value
      }

      object LazyCell {
        def apply[A](value: => A): LazyCell[A] = new LazyCell(value)
      }

      implicit class LazyCellAnyOps[A](key: A) {
        def ~>[B](value: => B): (A, LazyCell[B]) = key -> LazyCell(value)
      }

      implicit def LazyCell2Any[A](lazyCell: LazyCell[A]): A = lazyCell.get

      implicit def Any2LazyCell[A](value: => A): LazyCell[A] = LazyCell(value)

      type LazyMap[K, +V] = Map[K, LazyCell[V]]

      lazy val arrangements: LazyMap[Int, Long] =
        (allJolts
          .map(jolt =>
            jolt ~> (arrangements(jolt - 3) + arrangements(jolt - 2) + arrangements(jolt - 1))
          )
          .toMap + (0 ~> 1L))
          .withDefaultValue(0L)

      arrangements(builtinJolt)
    }
  }


  def parseJolts(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import MapKnotTyingPart2Solution._

    println(differencesProduct(parseJolts(input)))
    println(countArrangements(parseJolts(input)))
  }
}
