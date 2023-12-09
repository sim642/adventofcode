package eu.sim642.adventofcode2020

object Day6 {

  type Person = String
  type Group = Seq[Person]

  sealed trait Part {
    def countYesGroup(group: Group): Int

    def countYesGroups(groups: Seq[Group]): Int = groups.view.map(countYesGroup).sum
  }

  object Part1 extends Part {
    override def countYesGroup(group: Group): Int = group.map(_.toSet).reduce(_ ++ _).size
  }

  object Part2 extends Part {
    override def countYesGroup(group: Group): Int = group.map(_.toSet).reduce(_ & _).size
  }


  def parseGroup(s: String): Group = s.linesIterator.toSeq

  def parseGroups(input: String): Seq[Group] = input.split("\n\n").toSeq.map(parseGroup)

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countYesGroups(parseGroups(input)))
    println(Part2.countYesGroups(parseGroups(input)))
  }
}
