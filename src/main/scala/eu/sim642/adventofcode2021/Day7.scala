package eu.sim642.adventofcode2021

object Day7 {

  sealed trait Part {

    def fuel(crab: Int, pos: Int): Int

    def alignPosFuel(crabs: Seq[Int], pos: Int): Int = {
      crabs.view.map(fuel(_, pos)).sum
    }

    def minAlignPosFuel(crabs: Seq[Int]): Int = {
      (crabs.min to crabs.max).iterator.map(alignPosFuel(crabs, _)).min
    }
  }

  object Part1 extends Part {
    override def fuel(crab: Int, pos: Int): Int = (crab - pos).abs
  }

  object Part2 extends Part {
    override def fuel(crab: Int, pos: Int): Int = {
      val diff = Part1.fuel(crab, pos)
      (diff * (diff + 1)) / 2
    }
  }


  def parseCrabs(input: String): Seq[Int] = input.split(",").toSeq.map(_.toInt)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.minAlignPosFuel(parseCrabs(input)))
    println(Part2.minAlignPosFuel(parseCrabs(input)))
  }
}
