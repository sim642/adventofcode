package eu.sim642.adventofcode2021

object Day7 {

  def alignPosFuel(crabs: Seq[Int], pos: Int): Int = {
    crabs.view.map(crab => (crab - pos).abs).sum
  }

  def minAlignPosFuel(crabs: Seq[Int]): Int = {
    (crabs.min to crabs.max).iterator.map(alignPosFuel(crabs, _)).min
  }


  def parseCrabs(input: String): Seq[Int] = input.split(",").toSeq.map(_.toInt)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(minAlignPosFuel(parseCrabs(input)))
  }
}
