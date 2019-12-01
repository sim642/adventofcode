package eu.sim642.adventofcode2019

object Day1 {

  trait Part {
    def requiredFuel(mass: Int): Int

    def totalRequiredFuel(masses: Seq[Int]): Int = masses.map(requiredFuel).sum
  }

  object Part1 extends Part {
    override def requiredFuel(mass: Int): Int = mass / 3 - 2
  }

  trait Part2Solution extends Part

  object RecursivePart2Solution extends Part2Solution {
    override def requiredFuel(mass: Int): Int = {
      val fuel = Part1.requiredFuel(mass)
      if (fuel <= 0)
        0
      else
        fuel + requiredFuel(fuel)
    }
  }

  object ClosedFormPart2Solution extends Part2Solution {
    override def requiredFuel(mass: Int): Int = {
      val cols = (math.log((mass + 3.0) / 4) / math.log(3)).floor.toInt
      (1 to cols).map({ i =>
        val p = math.pow(3, i).toInt
        //0 max (((mass - (p - 3)) / p) - 2)
        0 max (((mass + 3.0) / p).floor.toInt - 3)
      }).sum
    }
  }

  def parseMasses(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.totalRequiredFuel(parseMasses(input)))
    println(RecursivePart2Solution.totalRequiredFuel(parseMasses(input)))
  }
}
