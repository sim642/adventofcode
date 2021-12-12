package eu.sim642.adventofcodelib

import OrderedSearch._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class OrderedSearchTest extends AnyFunSuite with ScalaCheckPropertyChecks {

  // TODO: custom generators
  // TODO: test specifically case where seq contains (multiple) x

  test("binaryLower") {
    forAll ("seq", "x") { (seq: Seq[Int], x: Int) =>
      val sortedSeq = seq.sorted

      val actualLower = binaryLower(sortedSeq, 0, sortedSeq.size)(x)
      val expectedLower = {
        val i = sortedSeq.indexWhere(_ >= x)
        if (i < 0)
          sortedSeq.size
        else
          i
      }
      assert(actualLower == expectedLower)
    }
  }

  test("binaryUpper") {
    forAll ("seq", "x") { (seq: Seq[Int], x: Int) =>
      val sortedSeq = seq.sorted

      val actualUpper = binaryUpper(sortedSeq, 0, sortedSeq.size)(x)
      val expectedUpper = {
        val i = sortedSeq.lastIndexWhere(_ <= x)
        if (i < 0)
          -1
        else
          i
      }
      assert(actualUpper == expectedUpper)
    }
  }

  test("binaryLower negative") {
    forAll ("seq", "x") { (seq: Seq[Int], x: Int) =>
      val sortedSeq = seq.sorted

      val actualLower = binaryLower((i: Int) => sortedSeq(i + sortedSeq.size), -sortedSeq.size, 0)(x)
      val expectedLower = {
        val i = sortedSeq.indexWhere(_ >= x)
        if (i < 0)
          0
        else
          i - sortedSeq.size
      }
      assert(actualLower == expectedLower)
    }
  }

  test("binaryUpper negative") {
    forAll ("seq", "x") { (seq: Seq[Int], x: Int) =>
      val sortedSeq = seq.sorted

      val actualUpper = binaryUpper((i: Int) => sortedSeq(i + sortedSeq.size), -sortedSeq.size, 0)(x)
      val expectedUpper = {
        val i = sortedSeq.lastIndexWhere(_ <= x)
        if (i < 0)
          -1 - sortedSeq.size
        else
          i - sortedSeq.size
      }
      assert(actualUpper == expectedUpper)
    }
  }
}
