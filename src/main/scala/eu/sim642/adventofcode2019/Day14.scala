package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.OrderedSearch

import Integral.Implicits._
import scala.annotation.tailrec

object Day14 {

  type Chemical = String
  type ChemicalAmounts = Map[Chemical, Long]
  type Reactions = Map[Chemical, (Long, ChemicalAmounts)]

  def oreForFuel(reactions: Reactions, fuelAmount: Long = 1): Long = {

    def helper(chemical: Chemical, amount: Long, excess: ChemicalAmounts): (Long, ChemicalAmounts) = chemical match {
      case "ORE" =>
        (amount, excess)
      case chemical =>
        val amountWithoutExcess = 0L max (amount - excess(chemical))
        val amountFromExcess = amount - amountWithoutExcess
        val excessWithoutAmount = excess + (chemical -> (excess(chemical) - amountFromExcess))

        val (outputAmount, inputChemicals) = reactions(chemical)
        val (reactionRepeat, outputExcess) = amountWithoutExcess /% outputAmount match {
          case (q, 0) => (q, 0L)
          case (q, r) => (q + 1, outputAmount - r)
        }

        // TODO: avoid recursion when amount will be 0 (reactionRepeat is 0)?
        val (ore, inputExcess) = inputChemicals.foldLeft((0L, excessWithoutAmount))({
          case ((ore, excess), (inputChemical, inputAmount)) =>
            val (inputOre, inputExcess) = helper(inputChemical, reactionRepeat * inputAmount, excess)
            (ore + inputOre, inputExcess)
        })

        (ore, inputExcess + (chemical -> (inputExcess(chemical) + outputExcess)))
    }

    helper("FUEL", fuelAmount, Map.empty.withDefaultValue(0))._1
  }

  def fuelForOre(reactions: Reactions, oreAmount: Long = 1000000000000L): Long = {
    OrderedSearch.exponentialBinaryUpper(oreForFuel(reactions, _), 0L)(oreAmount)
  }

  private val chemicalRegex = """(\d+) ([A-Z]+)""".r

  def parseReaction(s: String): (Chemical, (Long, ChemicalAmounts)) = {
    val chemicals = chemicalRegex.findAllMatchIn(s).map(m => m.group(2) -> m.group(1).toLong).toSeq
    val inputChemicals = chemicals.init.toMap
    val outputChemical = chemicals.last
    (outputChemical._1, (outputChemical._2, inputChemicals))
  }

  def parseReactions(input: String): Reactions = input.linesIterator.map(parseReaction).toMap

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(oreForFuel(parseReactions(input)))
    println(fuelForOre(parseReactions(input)))
  }
}
