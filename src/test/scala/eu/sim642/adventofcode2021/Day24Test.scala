package eu.sim642.adventofcode2021

import Day24._
import org.scalatest.funsuite.AnyFunSuite

class Day24Test extends AnyFunSuite {

  lazy val emilyskidsisterInput: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24emilyskidsister.txt")).mkString.trim
  lazy val dphilipsonInput: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24dphilipson.txt")).mkString.trim
  lazy val seligmanInput: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24seligman.txt")).mkString.trim
  lazy val aimorInput: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24aimor_.txt")).mkString.trim
  lazy val alexanderYuInput: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24alexander-yu.txt")).mkString.trim
  lazy val fireduck64Input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24fireduck64.txt")).mkString.trim
  lazy val firetechInput: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24firetech.txt")).mkString.trim
  lazy val mebeimInput: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24mebeim.txt")).mkString.trim

  test("Part 1 input answer") {
    assert(maxModelNumber(parseSteps(input)) == "97919997299495")
  }

  test("Part 2 input answer") {
    assert(minModelNumber(parseSteps(input)) == "51619131181131")
  }

  test("Part 1 emilyskidsister") {
    assert(maxModelNumber(parseSteps(emilyskidsisterInput)) == "79997391969649")
  }

  test("Part 2 emilyskidsister") {
    assert(minModelNumber(parseSteps(emilyskidsisterInput)) == "16931171414113")
  }

  test("Part 1 dphilipson") {
    assert(maxModelNumber(parseSteps(dphilipsonInput)) == "98491959997994")
  }

  test("Part 2 dphilipson") {
    assert(minModelNumber(parseSteps(dphilipsonInput)) == "61191516111321")
  }

  test("Part 1 seligman") {
    assert(maxModelNumber(parseSteps(seligmanInput)) == "96299896449997")
  }

  test("Part 2 seligman") {
    assert(minModelNumber(parseSteps(seligmanInput)) == "31162141116841")
  }

  test("Part 1 aimor") {
    assert(maxModelNumber(parseSteps(aimorInput)) == "92915979999498") // unconfirmed officially, confirmed by glguy, yitz
  }

  test("Part 2 aimor") {
    assert(minModelNumber(parseSteps(aimorInput)) == "21611513911181") // unconfirmed officially, confirmed by glguy, yitz
  }

  test("Part 1 alexander-yu") {
    assert(maxModelNumber(parseSteps(alexanderYuInput)) == "79197919993985") // unconfirmed officially, confirmed by glguy, yitz
  }

  test("Part 2 alexander-yu") {
    assert(minModelNumber(parseSteps(alexanderYuInput)) == "13191913571211") // unconfirmed officially, confirmed by glguy, yitz
  }

  test("Part 1 fireduck64") {
    assert(maxModelNumber(parseSteps(fireduck64Input)) == "99598963999971") // unconfirmed officially, confirmed by glguy, yitz
  }

  test("Part 2 fireduck64") {
    assert(minModelNumber(parseSteps(fireduck64Input)) == "93151411711211") // unconfirmed officially, confirmed by glguy, yitz
  }

  test("Part 1 firetech") {
    assert(maxModelNumber(parseSteps(firetechInput)) == "39494195799979") // unconfirmed officially, confirmed by glguy
  }

  test("Part 2 firetech") {
    assert(minModelNumber(parseSteps(firetechInput)) == "13161151139617") // unconfirmed officially, confirmed by glguy
  }

  test("Part 1 mebeim") {
    assert(maxModelNumber(parseSteps(mebeimInput)) == "92928914999991") // unconfirmed officially, confirmed by glguy, yitz
  }

  test("Part 2 mebeim") {
    assert(minModelNumber(parseSteps(mebeimInput)) == "91811211611981") // unconfirmed officially, confirmed by glguy, yitz
  }
}
