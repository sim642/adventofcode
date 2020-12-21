package eu.sim642.adventofcode2020

object Day21 {

  case class Food(ingredients: Set[String], allergens: Set[String])

  def findAllergenIngredients(foods: Seq[Food]): Map[String, Set[String]] = {
    (for {
      Food(ingredients, allergens) <- foods
      allergen <- allergens
    } yield allergen -> ingredients)
      .groupMapReduce(_._1)(_._2)(_ & _)
  }

  def findGoodIngredients(foods: Seq[Food]): Set[String] = {
    val allergenIngredients = findAllergenIngredients(foods)
    val allIngredients = foods.view.flatMap(_.ingredients).toSet
    val allAllergenIngredients = allergenIngredients.values.flatten.toSet
    allIngredients -- allAllergenIngredients
  }

  def countGoodIngredients(foods: Seq[Food]): Int = {
    val goodIngredients = findGoodIngredients(foods)
    foods
      .map(_.ingredients.count(goodIngredients))
      .sum
  }


  private val foodRegex = """(.*) \(contains (.*)\)""".r

  def parseFood(s: String): Food = s match {
    case foodRegex(ingredients, allergens) =>
      Food(
        ingredients.split(" ").toSet,
        allergens.split(", ").toSet
      )
  }

  def parseFoods(input: String): Seq[Food] = input.linesIterator.map(parseFood).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countGoodIngredients(parseFoods(input)))
  }
}
