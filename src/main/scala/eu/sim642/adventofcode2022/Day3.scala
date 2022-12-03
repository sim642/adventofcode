package eu.sim642.adventofcode2022

object Day3 {

  type Item = Char
  type Rucksack = (Set[Item], Set[Item])

  def itemPriority(item: Item): Int = {
    if (('a' to 'z').contains(item))
      item - 'a' + 1
    else if (('A' to 'Z').contains(item))
      item - 'A' + 27
    else
      throw new IllegalArgumentException("invalid item type")
  }

  def commonItemPrioritySum(rucksacks: Seq[Rucksack]): Int = {
    rucksacks
      .view
      .map(rucksack => itemPriority((rucksack._1 & rucksack._2).head))
      .sum
  }

  def parseRucksack(s: String): Rucksack = {
    val (x, y) = s.splitAt(s.length / 2)
    (x.toSet, y.toSet)
  }

  def parseRucksacks(input: String): Seq[Rucksack] = input.linesIterator.map(parseRucksack).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(commonItemPrioritySum(parseRucksacks(input)))
  }
}
