package eu.sim642.adventofcode2017

import eu.sim642.adventofcodelib.cycle.{FloydCycleFinder, NaiveCycleFinder}

import scala.collection.{AbstractIterator, mutable}

object Day6 {

  type Memory = IndexedSeq[Int]

  def reallocCycle(memory: Memory): Memory = {
    val (max, maxIndex) = memory.view.zipWithIndex.maxBy(_._1)
    val d = max / memory.size
    val r = max % memory.size
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

  object NaiveSolution extends Solution {
    // TODO: unused
    class ReallocIterator(private var memory: Memory) extends AbstractIterator[Memory] {
      override def hasNext: Boolean = true

      override def next(): Memory = {
        val returnMemory = memory
        memory = reallocCycle(memory)
        returnMemory
      }
    }

    override def reallocCycleCount(initialMemory: Memory): Int = {
      NaiveCycleFinder.find(initialMemory, reallocCycle).stemCycleLength
    }

    override def reallocCycleLoop(initialMemory: Memory): Int = {
      NaiveCycleFinder.find(initialMemory, reallocCycle).cycleLength
    }
  }

  object FloydSolution extends Solution {
    override def reallocCycleCount(initialMemory: Memory): Int = {
      FloydCycleFinder.find(initialMemory, reallocCycle).stemCycleLength
    }

    override def reallocCycleLoop(initialMemory: Memory): Int = {
      FloydCycleFinder.find(initialMemory, reallocCycle).cycleLength
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim
  lazy val inputSeq: IndexedSeq[Int] = input.split("\\s+").toIndexedSeq.map(_.toInt)

  def main(args: Array[String]): Unit = {
    import FloydSolution._

    println(reallocCycleCount(inputSeq))
    println(reallocCycleLoop(inputSeq))
  }
}
