package eu.sim642.adventofcode2020

object Day2 {

  case class Policy(min: Int, max: Int, char: Char)

  type Password = String

  def isValid(policy: Policy, password: Password): Boolean = {
    val count = password.count(_ == policy.char)
    policy.min <= count && count <= policy.max
  }

  def countValid(passwordPolicies: Seq[(Policy, Password)]): Int = passwordPolicies.count((isValid _).tupled)


  private val passwordPolicyRegex = """(\d+)-(\d+) (\w): (\w+)""".r

  def parsePasswordPolicy(s: String): (Policy, Password) = s match {
    case passwordPolicyRegex(min, max, char, password) =>
      (Policy(min.toInt, max.toInt, char.head), password)
  }

  def parsePasswordPolicies(input: String): Seq[(Policy, Password)] = input.linesIterator.map(parsePasswordPolicy).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countValid(parsePasswordPolicies(input)))
  }
}
