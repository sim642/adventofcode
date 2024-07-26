package eu.sim642.adventofcode2020

object Day2 {

  case class Policy(min: Int, max: Int, char: Char)

  type Password = String

  sealed trait Part {
    def isValid(policy: Policy, password: Password): Boolean

    def countValid(passwordPolicies: Seq[(Policy, Password)]): Int = passwordPolicies.count(isValid.tupled)
  }

  object Part1 extends Part {
    override def isValid(policy: Policy, password: Password): Boolean = {
      val Policy(min, max, char) = policy
      val count = password.count(_ == char)
      min <= count && count <= max
    }
  }

  object Part2 extends Part {
    override def isValid(policy: Policy, password: Password): Boolean = {
      val Policy(min, max, char) = policy
      (password(min - 1) == char) ^ (password(max - 1) == char)
    }
  }


  private val passwordPolicyRegex = """(\d+)-(\d+) (\w): (\w+)""".r

  def parsePasswordPolicy(s: String): (Policy, Password) = s match {
    case passwordPolicyRegex(min, max, char, password) =>
      (Policy(min.toInt, max.toInt, char.head), password)
  }

  def parsePasswordPolicies(input: String): Seq[(Policy, Password)] = input.linesIterator.map(parsePasswordPolicy).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countValid(parsePasswordPolicies(input)))
    println(Part2.countValid(parsePasswordPolicies(input)))
  }
}
