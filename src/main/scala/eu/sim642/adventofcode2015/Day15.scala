package eu.sim642.adventofcode2015

object Day15 {

  implicit class RepeatCombinationsSeqOps[A](s: Seq[A]) {
    def repeatCombinations(total: Int): Iterator[Seq[(A, Int)]] = {
      val hd +: tl = s: @unchecked
      if (tl.isEmpty)
        Iterator.single(Seq((hd, total)))
      else {
        for {
          hdCount <- (0 to total).iterator
          restCombination <- tl.repeatCombinations(total - hdCount)
        } yield (hd, hdCount) +: restCombination
      }
    }
  }

  case class Ingredient(capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int) {
    def *:(k: Int): Ingredient =
      Ingredient(k * capacity, k * durability, k * flavor, k * texture, k * calories)

    def +(that: Ingredient): Ingredient =
      Ingredient(capacity + that.capacity, durability + that.durability, flavor + that.flavor, texture + that.texture, calories + that.calories)

    def score: Int =
      (capacity max 0) * (durability max 0) * (flavor max 0) * (texture max 0)
  }

  def ingredientCombinations(ingredients: Seq[Ingredient], total: Int): Iterator[Ingredient] = {
    ingredients.repeatCombinations(total)
      .map({ ingredientCounts =>
        ingredientCounts.map({ case (ingredient, count) => count *: ingredient })
          .reduce(_ + _)
      })
  }

  def highestScore(ingredients: Seq[Ingredient]): Int = {
    ingredientCombinations(ingredients, 100)
      .map(_.score)
      .max
  }

  def highestScore(input: String): Int = highestScore(parseIngredients(input))

  def highestCalorieScore(ingredients: Seq[Ingredient]): Int = {
    ingredientCombinations(ingredients, 100)
      .filter(_.calories == 500)
      .map(_.score)
      .max
  }

  def highestCalorieScore(input: String): Int = highestCalorieScore(parseIngredients(input))


  private val ingredientRegex = """(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r

  def parseIngredient(s: String): Ingredient = s match {
    case ingredientRegex(name, capacity, durability, flavor, texture, calories) =>
      Ingredient(capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
  }

  def parseIngredients(input: String): Seq[Ingredient] = input.linesIterator.map(parseIngredient).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(highestScore(input))
    println(highestCalorieScore(input))
  }
}
