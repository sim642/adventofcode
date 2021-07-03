package eu.sim642.adventofcode2016

import scala.collection.mutable

object Day10 {

  sealed trait Target {
    val i: Int
  }
  case class Bot(i: Int) extends Target
  case class Output(i: Int) extends Target

  sealed trait Instruction
  case class ValueInstruction(bot: Bot, value: Int) extends Instruction
  case class CompareInstruction(bot: Bot, lowTarget: Target, highTarget: Target) extends Instruction

  private def execute(instructions: Seq[Instruction]) = {
    // TODO: fix all imperativeness
    val targets = mutable.Map[Target, Set[Int]]().withDefaultValue(Set.empty)

    instructions.foreach({
      case ValueInstruction(bot, value) =>
        targets += bot -> (targets(bot) + value)
      case _ =>
    })

    val comparisons = mutable.Map[Target, Set[Set[Int]]]().withDefaultValue(Set.empty)

    var changed: Boolean = true // always run loop body at least once, a la do..while, which is unsupported in Scala 3
    while (changed) {
      changed = false

      targets.find(_._2.size == 2) match {
        case Some((target, values)) =>
          instructions.find({
            case CompareInstruction(bot, _, _) => bot == target
            case _ => false
          }) match {
            case Some(CompareInstruction(bot, lowTarget, highTarget)) =>
              targets += lowTarget -> (targets(lowTarget) + values.min)
              targets += highTarget -> (targets(highTarget) + values.max)
              targets += bot -> Set()

              comparisons += bot -> (comparisons(bot) + values)

              changed = true
            case _ =>
          }
        case None =>
      }
    }

    //println(targets)
    //println(comparisons)
    (targets, comparisons)
  }

  def findComparer(instructions: Seq[Instruction], compareValues: Set[Int]): Int = {
    val comparisons = execute(instructions)._2
    comparisons.find(_._2.contains(compareValues)).get._1.i
  }

  def outputProduct(instructions: Seq[Instruction]): Int = {
    val targets = execute(instructions)._1
    targets(Output(0)).head * targets(Output(1)).head * targets(Output(2)).head
  }

  private val valueRegex = """value (\d+) goes to bot (\d+)""".r
  private val compareRegex = """bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""".r

  def parseTarget(targetStr: String, i: Int): Target = targetStr match {
    case "bot" => Bot(i)
    case "output" => Output(i)
  }

  def parseInstruction(str: String): Instruction = str match {
    case valueRegex(value, boti) =>
      ValueInstruction(Bot(boti.toInt), value.toInt)
    case compareRegex(boti, lowTarget, lowi, highTarget, highi) =>
      CompareInstruction(Bot(boti.toInt), parseTarget(lowTarget, lowi.toInt), parseTarget(highTarget, highi.toInt))
  }

  def parseInstructions(input: String): Seq[Instruction] = input.linesIterator.map(parseInstruction).toSeq

  def findComparer(input: String, compareValues: Set[Int] = Set(61, 17)): Int = findComparer(parseInstructions(input), compareValues)

  def outputProduct(input: String): Int = outputProduct(parseInstructions(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(findComparer(input))
    println(outputProduct(input))
  }
}
