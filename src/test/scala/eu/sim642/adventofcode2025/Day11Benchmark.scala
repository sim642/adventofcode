package eu.sim642.adventofcode2025

import Day11.*
import org.scalameter.Bench
import org.scalameter.api.Gen

object Day11Benchmark extends Bench.Group {

  val inputDevices = parseDevices(input)

  abstract class SolutionBenchmark(solution: Solution) extends Bench.LocalTime {
    performance of solution.getClass.getSimpleName in {
      performance of "Part 1" in {
        measure method "countPaths" in {
          using (Gen.unit("input")) in { _ =>
            solution.Part1.countPaths(inputDevices)
          }
        }
      }

      performance of "Part 2" in {
        measure method "countPaths" in {
          using (Gen.unit("input")) in { _ =>
            solution.Part2.countPaths(inputDevices)
          }
        }
      }
    }
  }

  class ViaMapSolutionBenchmark extends SolutionBenchmark(ViaMapSolution)

  class ViaPairSolutionBenchmark extends SolutionBenchmark(ViaPairSolution)

  class PermutationsSolutionBenchmark extends SolutionBenchmark(PermutationSolution)

  include(new ViaMapSolutionBenchmark)
  include(new ViaPairSolutionBenchmark)
  include(new PermutationsSolutionBenchmark)
}
