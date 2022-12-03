package eu.sim642.adventofcode2022

object Day3 {

  type Item = Char

  case class Rucksack(_1: Set[Item], _2: Set[Item]) {
    def all: Set[Item] = _1 ++ _2
  }

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

  def groupBadgePrioritySum(rucksacks: Seq[Rucksack]): Int = {
    rucksacks
      .view
      .grouped(3)
      .map(group => itemPriority(group.map(_.all).reduce(_ & _).head))
      .sum
  }

  def parseRucksack(s: String): Rucksack = {
    val (x, y) = s.splitAt(s.length / 2)
    Rucksack(x.toSet, y.toSet)
  }

  def parseRucksacks(input: String): Seq[Rucksack] = input.linesIterator.map(parseRucksack).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(commonItemPrioritySum(parseRucksacks(input)))
    println(groupBadgePrioritySum(parseRucksacks(input)))
  }
}
