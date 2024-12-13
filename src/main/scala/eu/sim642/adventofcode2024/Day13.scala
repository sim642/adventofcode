package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.graph.{Dijkstra, GraphSearch, TargetNode}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IntegralImplicits.*
import eu.sim642.adventofcodelib.NumberTheory

object Day13 {

  case class LongPos(x: Long, y: Long)

  case class ClawMachine(buttonA: Pos, buttonB: Pos, prize: LongPos)

  trait Part {
    def minWinTokens(clawMachine: ClawMachine): Long

    def sumMinWinTokens(clawMachines: Seq[ClawMachine]): Long = clawMachines.map(minWinTokens).sum
  }

  object Part1 extends Part {
    override def minWinTokens(clawMachine: ClawMachine): Long = {
      val ClawMachine(buttonA, buttonB, prize) = clawMachine

      /*val graphSearch = new GraphSearch[Pos] with TargetNode[Pos] {
        override val startNode: Pos = Pos.zero

        override def neighbors(pos: Pos): IterableOnce[(Pos, Int)] = {
          for {
            (button, tokens) <- Seq(clawMachine.buttonA -> 3, clawMachine.buttonB -> 1)
            newPos = pos + button
            if newPos <= targetNode
          } yield newPos -> tokens
        }

        override val targetNode: Pos = clawMachine.prize
      }

      Dijkstra.search(graphSearch).target.map(_._2).getOrElse(0)*/

      /*(for {
        timesA <- 0 to 100
        prizeB = prize - timesA *: buttonA
        timesB <- prizeB.x /! buttonB.x
        if timesB * buttonB.y == prizeB.y
      } yield 3 * timesA + timesB).minOption.getOrElse(0)*/

      (for {
        timesB <- (prize.x * buttonA.y - prize.y * buttonA.x) /! (buttonB.x * buttonA.y - buttonB.y * buttonA.x)
        timesA <- (prize.x - timesB * buttonB.x) /! buttonA.x
      } yield 3 * timesA + timesB).getOrElse(0)
    }
  }

  object Part2 extends Part {
    override def minWinTokens(clawMachine: ClawMachine): Long = {
      val newClawMachine = clawMachine.copy(prize = LongPos(clawMachine.prize.x + 10000000000000L, clawMachine.prize.y + 10000000000000L))
      Part1.minWinTokens(newClawMachine)
    }
  }

  def parseClawMachine(s: String): ClawMachine = s match {
    case s"Button A: X+$aX, Y+$aY\nButton B: X+$bX, Y+$bY\nPrize: X=$pX, Y=$pY" =>
      ClawMachine(Pos(aX.toInt, aY.toInt), Pos(bX.toInt, bY.toInt), LongPos(pX.toLong, pY.toLong))
  }

  def parseClawMachines(input: String): Seq[ClawMachine] = input.split("\n\n").map(parseClawMachine).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.sumMinWinTokens(parseClawMachines(input)))
    println(Part2.sumMinWinTokens(parseClawMachines(input)))
  }
}
