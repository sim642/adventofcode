package eu.sim642.adventofcodelib.cycle

import eu.sim642.adventofcode2018.Day2.HeadIterator

trait Cycle[A] {
  val stemLength: Int
  val cycleLength: Int
  val cycleHead: A

  def stemCycleLength: Int = stemLength + cycleLength
}

trait Indexing[A] { this: Cycle[A] =>
  def apply(i: Int): A
}

case class SimpleCycle[A](stemLength: Int, cycleLength: Int, cycleHead: A) extends Cycle[A]

case class FunctionCycle[A](stemLength: Int, cycleLength: Int, cycleHead: A)
                           (x0: A, f: A => A) extends Cycle[A] with Indexing[A] { // TODO: extract x0, f pair into separate structure?
  def apply(i: Int): A = {
    // TODO: optimize by starting from cycle head if i >= stemLength
    // TODO: add version of NaiveCycleFinder (or maybe even FloydCycleFinder) which keeps entire sequence for quick lookup here
    val shortI = stemLength + (i - stemLength) % cycleLength
    Iterator.iterate(x0)(f).drop(shortI).head
  }
}
