package eu.sim642.adventofcode2017

import eu.sim642.adventofcodelib.cycle.{BrentCycleFinder, FloydCycleFinder, FunctionCycleFinder, NaiveCycleFinder, NaiverCycleFinder}

import Integral.Implicits._

object Day6 {

  type Memory = IndexedSeq[Int]

  def reallocCycle(memory: Memory): Memory = {
    val (max, maxIndex) = memory.view.zipWithIndex.maxBy(_._1)
    val (d, r) = max /% memory.size
    val wrapAround: Boolean = maxIndex + r >= memory.size

    memory.updated(maxIndex, 0).map(_ + d).zipWithIndex.map({ case (x, i) =>
      if (maxIndex < i && i <= (maxIndex + r))
        x + 1
      else if (wrapAround && i <= (maxIndex + r) % memory.size)
        x + 1
      else
        x
    })
  }

  trait Solution {
    def reallocCycleCount(initialMemory: Memory): Int
    def reallocCycleLoop(initialMemory: Memory): Int
  }

  class FunctionCycleFinderSolution(cycleFinder: FunctionCycleFinder) extends Solution {
    override def reallocCycleCount(initialMemory: Memory): Int = {
      cycleFinder.find(initialMemory, reallocCycle).stemCycleLength
    }

    override def reallocCycleLoop(initialMemory: Memory): Int = {
      cycleFinder.find(initialMemory, reallocCycle).cycleLength
    }
  }

  object NaiveSolution extends FunctionCycleFinderSolution(NaiveCycleFinder)

  object NaiverSolution extends FunctionCycleFinderSolution(NaiverCycleFinder)

  object FloydSolution extends FunctionCycleFinderSolution(FloydCycleFinder)

  object BrentSolution extends FunctionCycleFinderSolution(BrentCycleFinder)

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim
  lazy val inputSeq: IndexedSeq[Int] = input.split("\\s+").toIndexedSeq.map(_.toInt)

  def main(args: Array[String]): Unit = {
    import BrentSolution._

    println(reallocCycleCount(inputSeq))
    println(reallocCycleLoop(inputSeq))
  }
}
