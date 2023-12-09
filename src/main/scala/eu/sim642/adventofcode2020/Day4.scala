package eu.sim642.adventofcode2020

object Day4 {

  type Passport = Map[String, String]

  sealed trait Part {
    def isValid(passport: Passport): Boolean

    def countValid(passports: Seq[Passport]): Int = passports.count(isValid)
  }

  object Part1 extends Part {
    private val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

    override def isValid(passport: Passport): Boolean = requiredFields.subsetOf(passport.keySet)
  }

  object Part2 extends Part {
    private val hgtRegex = """(\d+)(cm|in)""".r
    private val hclRegex = """#[0-9a-f]{6}""".r
    private val eclValues = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    private val pidRegex = """[0-9]{9}""".r

    val fieldValidators = Map(
      "byr" -> ((_: String).toIntOption.exists((1920 to 2002).contains)),
      "iyr" -> ((_: String).toIntOption.exists((2010 to 2020).contains)),
      "eyr" -> ((_: String).toIntOption.exists((2020 to 2030).contains)),
      "hgt" -> ((_: String) match {
        case hgtRegex(num, "cm") => (150 to 193).contains(num.toInt)
        case hgtRegex(num, "in") => (59 to 76).contains(num.toInt)
        case _ => false
      }),
      "hcl" -> (hclRegex.matches(_)),
      "ecl" -> (eclValues.contains(_)),
      "pid" -> (pidRegex.matches(_)),
    )

    override def isValid(passport: Passport): Boolean = {
      fieldValidators.forall({ case (field, validator) =>
        passport.contains(field) && validator(passport(field))
      })
    }
  }


  private val fieldRegex = """(.*):(.*)""".r

  def parseField(s: String): (String, String) = s match {
    case fieldRegex(key, value) => (key, value)
  }

  def parsePassport(s: String): Passport = s.split("\\s+").view.map(parseField).toMap

  def parsePassports(input: String): Seq[Passport] = input.split("\n\n").toSeq.map(parsePassport)

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countValid(parsePassports(input)))
    println(Part2.countValid(parsePassports(input)))
  }
}
