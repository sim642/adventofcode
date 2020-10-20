package eu.sim642.adventofcode2017

import Day20._
import eu.sim642.adventofcode2017.Day20Test._
import eu.sim642.adventofcodelib.pos.Pos3
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day20Test extends Suites(
  new BaseTest,
  new SimulateSolutionTest,
  new QuadraticSolutionTest
)

object Day20Test {

  class BaseTest extends AnyFunSuite {
    test("parseParticle") {
      assert(parseParticle("p=<2366,784,-597>, v=<-12,-41,50>, a=<-5,1,-2>") == Particle(Pos3(2366, 784, -597), Pos3(-12, -41, 50), Pos3(-5, 1, -2)))
    }
  }

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite {

    val exampleInput = """p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
                         |p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>""".stripMargin

    val exampleInput2 = """p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
                          |p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
                          |p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
                          |p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>""".stripMargin

    test("Part 1 example") {
      assert(solution.staysClosest(exampleInput) == 0)
    }

    test("Part 1 input answer") {
      assert(solution.staysClosest(input) == 170)
    }

    test("Part 2 example") {
      assert(solution.particlesLeft(exampleInput2) == 1)
    }

    test("Part 2 input answer") {
      assert(solution.particlesLeft(input) == 571)
    }
  }

  class SimulateSolutionTest extends SolutionTest(SimulateSolution)

  class QuadraticSolutionTest extends SolutionTest(QuadraticSolution) {
    import QuadraticSolution._

    test("Particle collides") {
      val p0 = parseParticle("p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>")
      val p1 = parseParticle("p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>")
      val p2 = parseParticle("p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>")

      assert((p0 collides p1) == Quadratic.Solutions(Set(2)))
      assert((p0 collides p2) == Quadratic.Solutions(Set(2)))
      assert((p1 collides p2) == Quadratic.Solutions(Set(2)))
    }
  }
}
