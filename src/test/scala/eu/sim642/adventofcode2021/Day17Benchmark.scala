package eu.sim642.adventofcode2021

import eu.sim642.adventofcode2021.Day17._
import org.scalameter.api._

object Day17Benchmark extends Bench.Group {

  sealed abstract class Part2SolutionBenchmark(part2Solution: Part2Solution) extends Bench.LocalTime {

    performance of part2Solution.getClass.getSimpleName in {
      measure method "countHitsTarget" in {
        using (Gen.unit("input")) in { _ =>
          part2Solution.countHitsTarget(parseTarget(input))
        }
      }
    }
  }

  class SimulatePart2SolutionBenchmark extends Part2SolutionBenchmark(SimulatePart2Solution)

  class AxisTimePart2SolutionBenchmark extends Part2SolutionBenchmark(AxisTimePart2Solution)

  include(new SimulatePart2SolutionBenchmark)
  include(new AxisTimePart2SolutionBenchmark)
}
