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
    // TODO: add version of NaiveCycleFinder (or maybe even FloydCycleFinder) which keeps entire sequence for quick lookup here
    if (i >= stemLength) {
      val shortI = (i - stemLength) % cycleLength
      Iterator.iterate(cycleHead)(f).drop(shortI).head
    }
    else
      Iterator.iterate(x0)(f).drop(i).head
  }
}
