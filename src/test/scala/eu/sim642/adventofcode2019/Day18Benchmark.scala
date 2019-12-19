package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2019.Day18._
import org.scalameter.api._

object Day18Benchmark extends Bench.Group {

  sealed abstract class SolutionBenchmark(solution: Solution) extends Bench.LocalTime {

    performance of solution.getClass.getSimpleName in {
      measure method "collectKeysSteps" in {
        using (Gen.unit("input")) in { _ =>
          solution.collectKeysSteps(parseInput(input))
        }
      }

      measure method "collectKeysStepsSplit" in {
        using (Gen.unit("input")) in { _ =>
          solution.collectKeysStepsSplit(parseInput(input))
        }
      }
    }
  }

  class BFSKeyNeighborsSolutionBenchmark extends SolutionBenchmark(BFSKeyNeighborsSolution)

  class FloydWarshallKeyNeighborsSolutionBenchmark extends SolutionBenchmark(FloydWarshallKeyNeighborsSolution)


  include(new BFSKeyNeighborsSolutionBenchmark)
  include(new FloydWarshallKeyNeighborsSolutionBenchmark)
}
