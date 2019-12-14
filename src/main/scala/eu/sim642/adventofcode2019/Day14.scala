package eu.sim642.adventofcode2019

import Integral.Implicits._

object Day14 {

  type Chemical = String
  type ChemicalAmounts = Map[Chemical, Int]
  type Reactions = Map[Chemical, (Int, ChemicalAmounts)]

  def oreForFuel(reactions: Reactions): Int = {

    def helper(chemical: Chemical, amount: Int, excess: ChemicalAmounts): (Int, ChemicalAmounts) = chemical match {
      case "ORE" =>
        (amount, excess)
      case chemical =>
        val amountWithoutExcess = 0 max (amount - excess(chemical))
        val amountFromExcess = amount - amountWithoutExcess
        val excessWithoutAmount = excess + (chemical -> (excess(chemical) - amountFromExcess))

        val (outputAmount, inputChemicals) = reactions(chemical)
        val (reactionRepeat, outputExcess) = amountWithoutExcess /% outputAmount match {
          case (q, 0) => (q, 0)
          case (q, r) => (q + 1, outputAmount - r)
        }

        val (ore, inputExcess) = inputChemicals.foldLeft((0, excessWithoutAmount))({
          case ((ore, excess), (inputChemical, inputAmount)) =>
            val (inputOre, inputExcess) = helper(inputChemical, reactionRepeat * inputAmount, excess)
            (ore + inputOre, inputExcess)
        })

        (ore, inputExcess + (chemical -> (inputExcess(chemical) + outputExcess)))
    }

    helper("FUEL", 1, Map.empty.withDefaultValue(0))._1
  }

  private val chemicalRegex = """(\d+) ([A-Z]+)""".r

  def parseReaction(s: String): (Chemical, (Int, ChemicalAmounts)) = {
    val chemicals = chemicalRegex.findAllMatchIn(s).map(m => m.group(2) -> m.group(1).toInt).toSeq
    val inputChemicals = chemicals.init.toMap
    val outputChemical = chemicals.last
    (outputChemical._1, (outputChemical._2, inputChemicals))
  }

  def parseReactions(input: String): Reactions = input.linesIterator.map(parseReaction).toMap

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(oreForFuel(parseReactions(input)))
  }
}
