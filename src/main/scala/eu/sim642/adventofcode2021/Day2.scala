package eu.sim642.adventofcode2021

import Day2.Command._
import eu.sim642.adventofcodelib.pos.Pos

object Day2 {

  enum Command {
    case Forward(amount: Int)
    case Down(amount: Int)
    case Up(amount: Int)
  }

  sealed trait Part {
    type Submarine

    protected val initialSubmarine: Submarine

    protected def applyCommand(submarine: Submarine, command: Command): Submarine

    protected def submarinePos(submarine: Submarine): Pos

    def multiplyFinalPos(commands: Seq[Command]): Int = {
      val finalSubmarine = commands.foldLeft(initialSubmarine)(applyCommand)
      val finalPos = submarinePos(finalSubmarine)
      finalPos.x * finalPos.y
    }
  }

  object Part1 extends Part {
    override type Submarine = Pos

    override protected val initialSubmarine: Pos = Pos.zero

    override protected def applyCommand(pos: Pos, command: Command): Pos = {
      val delta = command match {
        case Forward(amount) => Pos(amount, 0)
        case Down(amount) => Pos(0, amount)
        case Up(amount) => Pos(0, -amount)
      }
      pos + delta
    }

    override protected def submarinePos(pos: Pos): Pos = pos
  }

  object Part2 extends Part {
    override case class Submarine(pos: Pos, aim: Int)

    override protected val initialSubmarine: Submarine = Submarine(Pos.zero, 0)

    override protected def applyCommand(submarine: Submarine, command: Command): Submarine = {
      val Submarine(pos, aim) = submarine
      command match {
        case Forward(amount) => submarine.copy(pos = pos + Pos(amount, aim * amount))
        case Down(amount) => submarine.copy(aim = aim + amount)
        case Up(amount) => submarine.copy(aim = aim - amount)
      }
    }

    override protected def submarinePos(submarine: Submarine): Pos = submarine.pos
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

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.multiplyFinalPos(parseCommands(input)))
    println(Part2.multiplyFinalPos(parseCommands(input)))
  }
}
