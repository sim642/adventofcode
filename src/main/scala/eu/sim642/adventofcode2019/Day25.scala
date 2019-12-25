package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2019.Day9._

object Day25 {

  def runInteractive(): Unit = {
    val reader = Console.in
    val inputs = LazyList.unfold(())({ _ =>
      val c = reader.read()
      if (c < 0)
        None
      else
        Some((c.toLong, ()))
    })

    val outputs = ProgramState(parseProgram(input), inputs = inputs).outputs
    outputs.foreach(c => print(c.toChar))
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    runInteractive()

    // whirled peas, fixed point, prime number, antenna
    // 2622472
  }
}
