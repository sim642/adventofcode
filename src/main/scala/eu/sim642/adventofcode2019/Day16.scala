package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.IterableImplicits._
import Integral.Implicits._

object Day16 {

  type Signal = IndexedSeq[Int]

  trait Solution {
    def stepPhasesEight(signal: Signal, phases: Int = 100): String
    def stepPhasesEightOffset(signal: Signal, phases: Int = 100): String
  }

  trait StepPhaseSolution extends Solution {
    def stepPhase(offset: Int)(signal: Signal): Signal

    def stepPhases(signal: Signal, phases: Int, offset: Int = 0): Signal = {
      Iterator.iterate(signal)(stepPhase(offset))(phases)
    }

    override def stepPhasesEight(signal: Signal, phases: Int): String = {
      stepPhases(signal, phases).take(8).mkString
    }

    override def stepPhasesEightOffset(signal: Signal, phases: Int): String = {
      val messageOffset = signal.take(7).mkString.toInt
      val (q, r) = messageOffset /% signal.length // avoid constructing prefix of string which will be dropped anyway
      val initialSignal = Iterator.fill(10000 - q)(signal).flatten.drop(r).toIndexedSeq
      stepPhases(initialSignal, phases, messageOffset).take(8).mkString
    }
  }

  private val basePattern = Seq(0, 1, 0, -1)

  object NaiveSolution extends StepPhaseSolution {
    override def stepPhase(offset: Int)(signal: Signal): Signal = {
      IndexedSeq.tabulate(signal.length)({ i =>
        val repeat = (offset + i) + 1
        val pattern = basePattern.view.flatMap(Iterator.fill(repeat)(_))
        val sum = (signal.view zip pattern.cycle.drop(offset + 1)).map({ case (a, b) => a * b}).sum
        (sum % 10).abs
      })
    }
  }

  /**
    * Optimization of naive solution by specializing for [[basePattern]] positive and negative ranges.
    * Ranges of 0-s can be ignored, other ranges can be calculated by prefix sums.
    */
  object RangeSolution extends StepPhaseSolution {
    override def stepPhase(offset: Int)(signal: Signal): Signal = {
      val totalLength = offset + signal.length

      val prefixSum: Signal = signal.scanLeft(0)(_ + _)

      def ranges(repeat: Int, baseI: Int): Iterator[Range] = {
        Iterator.from(0)
          .map(i => 4 * i * repeat - 1)
          .map(baseStart => baseStart + baseI * repeat)
          .takeWhile(_ < signal.length + offset)
          .map(start => start until ((start + repeat) min totalLength))
      }

      def rangeSum(range: Range): Int = {
        prefixSum(range.max + 1 - offset) - prefixSum(range.min - offset)
      }

      IndexedSeq.tabulate(signal.length)({ i =>
        val repeat = (offset + i) + 1
        val posSum = ranges(repeat, 1).map(rangeSum).sum
        val negSum = ranges(repeat, 3).map(rangeSum).sum
        val sum = posSum - negSum
        (sum % 10).abs
      })
    }
  }

  /**
    * Optimization of [[RangeSolution]] by specializing for offsets beyond signal midpoint.
    * In such case there is only one positive range to the end and no negative ranges.
    */
  object UpperTriangularSolution extends StepPhaseSolution {
    def upperTriangularStepPhase(offset: Int)(signal: Signal): Signal = {
      require(offset >= signal.length)

      val prefixSum: Signal = signal.scanLeft(0)(_ + _)

      IndexedSeq.tabulate(signal.length)({ i =>
        val sum = prefixSum.last - prefixSum(i)
        (sum % 10).abs
      })
    }

    override def stepPhase(offset: Int)(signal: Signal): Signal = {
      if (offset >= signal.length)
        upperTriangularStepPhase(offset)(signal)
      else
        RangeSolution.stepPhase(offset)(signal) // fall back for part 1
    }
  }

  def parseSignal(input: String): Signal = input.map(_.asDigit)

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import UpperTriangularSolution._

    println(stepPhasesEight(parseSignal(input)))
    println(stepPhasesEightOffset(parseSignal(input)))
  }
}
