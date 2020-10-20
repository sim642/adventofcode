package eu.sim642.adventofcodelib

import IterableOnceImplicits._
import org.scalatest.funsuite.AnyFunSuite

class IterableOnceImplicitsTest extends AnyFunSuite {

  //noinspection EmptyCheck,OptionEqualsSome
  test("minStrict") {
    assert(Seq.empty[Int].minStrict == None)

    assert(Seq(1).minStrict == Some(1))

    assert(Seq(1, 2).minStrict == Some(1))
    assert(Seq(2, 1).minStrict == Some(1))
    assert(Seq(1, 1).minStrict == None)

    assert(Seq(1, 2, 3).minStrict == Some(1))
    assert(Seq(2, 1, 3).minStrict == Some(1))
    assert(Seq(3, 2, 1).minStrict == Some(1))
    assert(Seq(1, 2, 2).minStrict == Some(1))
    assert(Seq(2, 1, 2).minStrict == Some(1))
    assert(Seq(2, 2, 1).minStrict == Some(1))
    assert(Seq(1, 1, 2).minStrict == None)
    assert(Seq(2, 1, 1).minStrict == None)
    assert(Seq(1, 2, 1).minStrict == None)
  }
}
