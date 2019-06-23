package eu.sim642.adventofcode2016

object Day2 {

  type Move = Char

  case class Button(n: Int) {
    def move(move: Move): Button = move match {
      case 'U' if n >= 4 => Button(n - 3)
      case 'D' if n <= 6 => Button(n + 3)
      case 'L' if n % 3 != 1 => Button(n - 1)
      case 'R' if n % 3 != 0 => Button(n + 1)
      case _ => this
    }

    def move(moves: Seq[Move]): Button = moves.foldLeft(this)(_.move(_))
  }

  def bathroomCode(allMoves: Seq[String]): String = {
    allMoves.scanLeft(Button(5))(_.move(_)).map(_.n).tail.mkString("")
  }

  def bathroomCode(input: String): String = bathroomCode(input.lines.toSeq)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(bathroomCode(input))
  }
}
