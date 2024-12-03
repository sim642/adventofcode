package eu.sim642.adventofcode2024

object Day3 {

  trait Part {
    def sumMuls(s: String): Int
  }

  object Part1 extends Part {
    private val mulRegex = """mul\((\d{1,3}),(\d{1,3})\)""".r

    override def sumMuls(s: String): Int = {
      mulRegex.findAllMatchIn(s)
        .map(m => m.group(1).toInt * m.group(2).toInt)
        .sum
    }
  }

  object Part2 extends Part {
    private val mulRegex = """mul\((\d{1,3}),(\d{1,3})\)|do(n't)?\(\)""".r

    override def sumMuls(s: String): Int = {
      mulRegex.findAllMatchIn(s)
        .foldLeft((true, 0))({ case ((enabled, sum), m) =>
          (enabled, m.matched) match {
            case (_, "do()") => (true, sum)
            case (_, "don't()") => (false, sum)
            case (true, _) => (enabled, sum + m.group(1).toInt * m.group(2).toInt)
            case (false, _) => (enabled, sum)
          }
        })
        ._2
    }
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.sumMuls(input))
    println(Part2.sumMuls(input))
  }
}
