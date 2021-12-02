package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.pos.Pos

object Day2 {

  sealed trait Command
  case class Forward(amount: Int) extends Command
  case class Down(amount: Int) extends Command
  case class Up(amount: Int) extends Command

  def applyCommand(pos: Pos, command: Command): Pos = {
    val delta = command match {
      case Forward(amount) => Pos(amount, 0)
      case Down(amount) => Pos(0, amount)
      case Up(amount) => Pos(0, -amount)
    }
    pos + delta
  }

  def multiplyFinalPos(commands: Seq[Command]): Int = {
    val finalPos = commands.foldLeft(Pos.zero)(applyCommand)
    finalPos.x * finalPos.y
  }

  private val commandRegex = """(forward|down|up) (\d+)""".r

  def parseCommand(s: String): Command = s match {
    case commandRegex(command, amountStr) =>
      val amount = amountStr.toInt
      command match {
        case "forward" => Forward(amount)
        case "down" => Down(amount)
        case "up" => Up(amount)
      }
  }

  def parseCommands(input: String): Seq[Command] = input.linesIterator.map(parseCommand).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(multiplyFinalPos(parseCommands(input)))
  }
}
