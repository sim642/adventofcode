package eu.sim642.adventofcode2019

object Day1 {

  trait Part {
    def requiredFuel(mass: Int): Int

    def totalRequiredFuel(masses: Seq[Int]): Int = masses.map(requiredFuel).sum
  }

  object Part1 extends Part {
    override def requiredFuel(mass: Int): Int = mass / 3 - 2
  }

  object Part2 extends Part {
    override def requiredFuel(mass: Int): Int = {
      val fuel = Part1.requiredFuel(mass)
      if (fuel <= 0)
        0
      else
        fuel + requiredFuel(fuel)
    }
  }

  def parseMasses(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.totalRequiredFuel(parseMasses(input)))
    println(Part2.totalRequiredFuel(parseMasses(input)))
  }
}
