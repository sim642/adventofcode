package eu.sim642.adventofcode2024

import Day22.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day22Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  val exampleInput =
    """1
      |10
      |100
      |2024""".stripMargin

  val exampleInput2 =
    """1
      |2
      |3
      |2024""".stripMargin

  test("Part 1 examples") {
    val secrets = Table(
      "secret",
      123,
      15887950,
      16495136,
      527345,
      704524,
      1553684,
      12683156,
      11100544,
      12249484,
      7753432,
      5908254,
    )

    val it = secretIterator(123)
    forAll (secrets) { secret =>
      assert(it.next() == secret)
    }

    assert(sumSecretsAfter(parseSecrets(exampleInput)) == 37327623)
  }

  test("Part 1 input answer") {
    assert(sumSecretsAfter(parseSecrets(input)) == 21147129593L)
  }

  test("Part 2 examples") {
    assert(mostBananas(parseSecrets(exampleInput2)) == 23)
  }

  test("Part 2 input answer") {
    assert(mostBananas(parseSecrets(input)) == 2445)
  }
}
