package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.IntegralImplicits.*
import eu.sim642.adventofcodelib.pos.Pos

object Day13 {

  case class LongPos(x: Long, y: Long) // TODO: generalize library

  case class ClawMachine(buttonA: Pos, buttonB: Pos, prize: LongPos)

  trait Part {
    def minWinTokens(clawMachine: ClawMachine): Option[Long]

    def sumMinWinTokens(clawMachines: Seq[ClawMachine]): Long = clawMachines.flatMap(minWinTokens).sum
  }

  object Part1 extends Part {
    override def minWinTokens(clawMachine: ClawMachine): Option[Long] = {
      val ClawMachine(buttonA, buttonB, prize) = clawMachine
      // linear system of 2 equations
      for {
        timesB <- (prize.x * buttonA.y - prize.y * buttonA.x) /! (buttonB.x * buttonA.y - buttonB.y * buttonA.x)
        timesA <- (prize.x - timesB * buttonB.x) /! buttonA.x
      } yield 3 * timesA + timesB
    }
  }

  object Part2 extends Part {
    override def minWinTokens(clawMachine: ClawMachine): Option[Long] = {
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
