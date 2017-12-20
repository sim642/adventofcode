package eu.sim642.adventofcode2017

import Day20._
import org.scalatest.FunSuite

class Day20Test extends FunSuite {

  val exampleInput = """p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
                       |p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>""".stripMargin

  val exampleInput2 = """p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
                        |p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
                        |p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
                        |p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>""".stripMargin

  test("parseParticle") {
    assert(parseParticle("p=<2366,784,-597>, v=<-12,-41,50>, a=<-5,1,-2>") == Particle(Pos3(2366, 784, -597), Pos3(-12, -41, 50), Pos3(-5, 1, -2)))
  }

  test("Part 1 example") {
    assert(staysClosest(exampleInput) == 0)
  }

  test("Part 1 input answer") {
    assert(staysClosest(input) == 170)
  }

  test("Particle collides") {
    val p0 = parseParticle("p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>")
    val p1 = parseParticle("p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>")
    val p2 = parseParticle("p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>")

    assert((p0 collides p1) == Quadratic.Solutions(Set(2)))
    assert((p0 collides p2) == Quadratic.Solutions(Set(2)))
    assert((p1 collides p2) == Quadratic.Solutions(Set(2)))
  }

  test("Part 2 example") {
    assert(particlesLeft(exampleInput2) == 1)
  }
}
