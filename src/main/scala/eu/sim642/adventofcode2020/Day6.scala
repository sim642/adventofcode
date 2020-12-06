package eu.sim642.adventofcode2020

object Day6 {

  type Person = String
  type Group = Seq[Person]

  def countYesGroup(group: Group): Int = group.map(_.toSet).reduce(_ ++ _).size

  def countYesGroups(groups: Seq[Group]): Int = groups.view.map(countYesGroup).sum


  def parseGroup(s: String): Group = s.linesIterator.toSeq

  def parseGroups(input: String): Seq[Group] = input.split("\n\n").map(parseGroup)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countYesGroups(parseGroups(input)))
  }
}
