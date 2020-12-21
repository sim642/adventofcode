package eu.sim642.adventofcode2020

import Day21._
import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends AnyFunSuite {

  val exampleInput =
    """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
      |trh fvjkl sbzzf mxmxvkd (contains dairy)
      |sqjhc fvjkl (contains soy)
      |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin

  test("Part 1 examples") {
    assert(countGoodIngredients(parseFoods(exampleInput)) == 5)
  }

  test("Part 1 input answer") {
    assert(countGoodIngredients(parseFoods(input)) == 2170)
  }

  test("Part 2 examples") {
    assert(canonicalBadIngredients(parseFoods(exampleInput)) == "mxmxvkd,sqjhc,fvjkl")
  }

  test("Part 2 input answer") {
    assert(canonicalBadIngredients(parseFoods(input)) == "nfnfk,nbgklf,clvr,fttbhdr,qjxxpr,hdsm,sjhds,xchzh")
  }
}
