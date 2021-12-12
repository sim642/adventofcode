package eu.sim642.adventofcode2021

import Day12._
import org.scalameter.api._

object Day12Benchmark extends Bench.LocalTime {

  performance of "Day12" in {
    performance of "Part1" in {
      measure method "countPaths" in {
        using (Gen.unit("input")) in { _ =>
          Part1.countPaths(parseCaveMap(input))
        }
      }
    }

    performance of "Part2" in {
      measure method "countPaths" in {
        using (Gen.unit("input")) in { _ =>
          Part2.countPaths(parseCaveMap(input))
        }
      }
    }
  }
}
