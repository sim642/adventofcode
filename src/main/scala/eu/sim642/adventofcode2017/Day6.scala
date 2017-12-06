package eu.sim642.adventofcode2017

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
    class ReallocIterator(private var memory: Memory) extends AbstractIterator[Memory] {
      override def hasNext: Boolean = true

      override def next(): Memory = {
        val returnMemory = memory
        memory = reallocCycle(memory)
        returnMemory
      }
    }

    override def reallocCycleCount(initialMemory: Memory): Int = {
      val it = new ReallocIterator(initialMemory)
      val prevMemories = mutable.Set[Memory]()

      while (prevMemories.add(it.next())) {}

      prevMemories.size
    }

    override def reallocCycleLoop(initialMemory: Memory): Int = {
      val it = new ReallocIterator(initialMemory)
      val prevMemories = mutable.Map[Memory, Int]()

      var n = 0
      var prevN: Option[Int] = None
      do {
        val memory = it.next()
        prevN = prevMemories.put(memory, n)
        n += 1
      } while (prevN.isEmpty)

      n - prevN.get - 1
    }
  }

  object FloydSolution extends Solution {
    /**
      * https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare
      */
    def floyd[A](x0: A, f: A => A): (Int, Int) = {
      var tortoise = f(x0)
      var hare = f(f(x0))
      while (tortoise != hare) {
        tortoise = f(tortoise)
        hare = f(f(hare))
      }

      var mu = 0
      tortoise = x0
      while (tortoise != hare) {
        tortoise = f(tortoise)
        hare = f(hare)
        mu += 1
      }

      var lam = 1
      hare = f(tortoise)
      while (tortoise != hare) {
        hare = f(hare)
        lam += 1
      }

      (mu, lam)
    }

    override def reallocCycleCount(initialMemory: Memory): Int = {
      val (mu, lam) = floyd(initialMemory, reallocCycle)
      mu + lam
    }

    override def reallocCycleLoop(initialMemory: Memory): Int = {
      val (mu, lam) = floyd(initialMemory, reallocCycle)
      lam
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
