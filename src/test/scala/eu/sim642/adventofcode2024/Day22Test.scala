package eu.sim642.adventofcode2024

import Day22._
import org.scalatest.funsuite.AnyFunSuite

class Day22Test extends AnyFunSuite {

  val exampleInput =
    """1
      |10
      |100
      |2024""".stripMargin

  test("Part 1 examples") {
    assert(secretAfter(123, 1) == 15887950)
    assert(secretAfter(123, 2) == 16495136)
    assert(secretAfter(123, 3) == 527345)
    assert(secretAfter(123, 4) == 704524)
    assert(secretAfter(123, 5) == 1553684)
    assert(secretAfter(123, 6) == 12683156)
    assert(secretAfter(123, 7) == 11100544)
    assert(secretAfter(123, 8) == 12249484)
    assert(secretAfter(123, 9) == 7753432)
    assert(secretAfter(123, 10) == 5908254)
    assert(sumSecretsAfter(parseSecrets(exampleInput)) == 37327623)
  }

  test("Part 1 input answer") {
    assert(sumSecretsAfter(parseSecrets(input)) == 21147129593L)
  }
}
