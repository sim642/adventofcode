package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.OrderedSearch

object Day7 {

  sealed trait PartFuel {
    def fuel(crab: Int, pos: Int): Int

    def alignPosFuel(crabs: Seq[Int], pos: Int): Int = {
      crabs.view.map(fuel(_, pos)).sum
    }
  }

  sealed trait Part1Fuel extends PartFuel {
    override def fuel(crab: Int, pos: Int): Int = (crab - pos).abs
  }

  object Part1Fuel extends Part1Fuel // for testing

  sealed trait Part2Fuel extends PartFuel {
    override def fuel(crab: Int, pos: Int): Int = {
      val diff = Part1Fuel.fuel(crab, pos)
      (diff * (diff + 1)) / 2
    }
  }

  object Part2Fuel extends Part2Fuel // for testing

  sealed trait PartSolution extends PartFuel {
    def minAlignPosFuel(crabs: Seq[Int]): Int
  }

  sealed trait Solution {
    val Part1: PartSolution
    val Part2: PartSolution
  }

  /**
   * Solution, which linearly searches the min-max range for the optimum.
   */
  object LinearSolution extends Solution {

    sealed trait LinearPartSolution extends PartSolution {
      override def minAlignPosFuel(crabs: Seq[Int]): Int = {
        (crabs.min to crabs.max).iterator.map(alignPosFuel(crabs, _)).min
      }
    }

    override object Part1 extends LinearPartSolution with Part1Fuel
    override object Part2 extends LinearPartSolution with Part2Fuel
  }

  /**
   * Solution, which binary searches the min-max range for the optimum using difference.
   */
  object BinarySolution extends Solution {

    sealed trait BinaryPartSolution extends PartSolution {
      override def minAlignPosFuel(crabs: Seq[Int]): Int = {
        def f(pos: Int): Int = alignPosFuel(crabs, pos + 1) - alignPosFuel(crabs, pos)
        val pos = OrderedSearch.binaryLower(f, crabs.min, crabs.max + 1)(0)
        alignPosFuel(crabs, pos)
      }
    }

    override object Part1 extends BinaryPartSolution with Part1Fuel
    override object Part2 extends BinaryPartSolution with Part2Fuel
  }

  /**
   * Solution, which uses math to compute the optimum.
   */
  object MathSolution extends Solution {

    override object Part1 extends PartSolution with Part1Fuel {
      override def minAlignPosFuel(crabs: Seq[Int]): Int = {
        val pos = crabs.sorted.apply(crabs.size / 2) // median
        alignPosFuel(crabs, pos)
      }
    }

    override object Part2 extends PartSolution with Part2Fuel {
      override def minAlignPosFuel(crabs: Seq[Int]): Int = {
        // mniip's solution
        val sum = crabs.sum
        val n = crabs.size
        val pos = (sum + crabs.count(n * _ > sum)) / n // mean and extra for above mean
        alignPosFuel(crabs, pos)
      }
    }
  }


  def parseCrabs(input: String): Seq[Int] = input.split(",").toSeq.map(_.toInt)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import LinearSolution._

    println(Part1.minAlignPosFuel(parseCrabs(input)))
    println(Part2.minAlignPosFuel(parseCrabs(input)))
  }
}
