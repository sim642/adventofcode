package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.graph.{Dijkstra, GraphSearch, TargetNode}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IntegralImplicits._

object Day13 {

  case class ClawMachine(buttonA: Pos, buttonB: Pos, prize: Pos)

  def minWinTokens(clawMachine: ClawMachine): Int = {
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

    (for {
      timesA <- 0 to 100
      prizeB = prize - timesA *: buttonA
      timesB <- prizeB.x /! buttonB.x
      if timesB * buttonB.y == prizeB.y
    } yield 3 * timesA + timesB).minOption.getOrElse(0)
  }

  def sumMinWinTokens(clawMachines: Seq[ClawMachine]): Int = clawMachines.map(minWinTokens).sum

  def parseClawMachine(s: String): ClawMachine = s match {
    case s"Button A: X+$aX, Y+$aY\nButton B: X+$bX, Y+$bY\nPrize: X=$pX, Y=$pY" =>
      ClawMachine(Pos(aX.toInt, aY.toInt), Pos(bX.toInt, bY.toInt), Pos(pX.toInt, pY.toInt))
  }

  def parseClawMachines(input: String): Seq[ClawMachine] = input.split("\n\n").map(parseClawMachine).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumMinWinTokens(parseClawMachines(input)))
  }
}
