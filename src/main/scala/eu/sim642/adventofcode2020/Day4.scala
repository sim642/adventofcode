package eu.sim642.adventofcode2020

object Day4 {

  type Passport = Map[String, String]

  private val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def isValid(passport: Passport): Boolean = requiredFields.subsetOf(passport.keySet)

  def countValid(passports: Seq[Passport]): Int = passports.count(isValid)


  private val fieldRegex = """(.*):(.*)""".r

  def parseField(s: String): (String, String) = s match {
    case fieldRegex(key, value) => (key, value)
  }

  def parsePassport(s: String): Passport = s.split("\\s+").map(parseField).toMap

  def parsePassports(input: String): Seq[Passport] = input.split("\n\n").map(parsePassport)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countValid(parsePassports(input)))
  }
}
