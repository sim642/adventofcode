package eu.sim642.adventofcode2017

object Day20 {

  case class Pos3(x: Int, y: Int, z: Int) {
    def manhattanDistance(that: Pos3): Int =
      (x - that.x).abs + (y - that.y).abs + (z - that.z).abs

    def +(that: Pos3): Pos3 =
      Pos3(x + that.x, y + that.y, z + that.z)
  }

  case class Particle(p: Pos3, v: Pos3, a: Pos3)

  private val particleRegex = """p=<\s*(-?\d+),\s*(-?\d+),\s*(-?\d+)>, v=<\s*(-?\d+),\s*(-?\d+),\s*(-?\d+)>, a=<\s*(-?\d+),\s*(-?\d+),\s*(-?\d+)>""".r

  def parseParticle(str: String): Particle = str match {
    case particleRegex(px, py, pz, vx, vy, vz, ax, ay, az) =>
      Particle(Pos3(px.toInt, py.toInt, pz.toInt), Pos3(vx.toInt, vy.toInt, vz.toInt), Pos3(ax.toInt, ay.toInt, az.toInt))
  }

  def parseParticles(input: String): Seq[Particle] = input.lines.map(parseParticle).toSeq


  trait Solution {
    def staysClosest(particles: Seq[Particle]): Int
    def staysClosest(input: String): Int = staysClosest(parseParticles(input))

    def particlesLeft(particles: Seq[Particle]): Int
    def particlesLeft(input: String): Int = particlesLeft(parseParticles(input))
  }

  object SimulateSolution extends Solution {

    implicit class ParticleUpdate(particle: Particle) {
      private val Particle(p, v, a) = particle

      def updated: Particle = {
        val v2 = v + a
        Particle(p + v2, v2, a)
      }
    }

    val maxIterations = 1000

    override def staysClosest(particles: Seq[Particle]): Int = {
      val endParticles = (0 until maxIterations).foldLeft(particles.toVector)({ case (acc, _) => acc.map(_.updated) })
      endParticles.zipWithIndex.minBy({ case (p, i) => p.p manhattanDistance Pos3(0, 0, 0) })._2
    }

    override def particlesLeft(particles: Seq[Particle]): Int = {
      val endParticles = (0 until maxIterations).foldLeft(particles.toSet)({ case (acc, _) =>
        val collided = acc.groupBy(_.p).filter(_._2.size > 1).flatMap(_._2).toSet
        (acc -- collided).map(_.updated)
      })
      endParticles.size
    }
  }

  object QuadraticSolution extends Solution {

    object Quadratic {
      sealed trait Solution[+A]
      case class Solutions[A](sols: Set[A]) extends Solution[A]
      case object AnySolution extends Solution[Nothing]

      def solve(a: Double, b: Double, c: Double): Solution[Double] = {
        if (a != 0) {
          val D = b * b - 4 * a * c
          if (D >= 0) {
            val d = Math.sqrt(D)
            Solutions(Set((-b - d) / (2 * a), (-b + d) / (2 * a)))
          }
          else
            Solutions(Set())
        }
        else if (b != 0)
          Solutions(Set(-c / b))
        else if (c != 0)
          Solutions(Set())
        else
          AnySolution
      }

      def solveInt(a: Double, b: Double, c: Double): Solution[Int] = solve(a, b, c) match {
        case Solutions(sols) => Solutions(sols.filter(_.isWhole).map(_.toInt))
        case AnySolution => AnySolution
      }

      def solveSimInt(coeffs: Seq[(Double, Double, Double)]): Solution[Int] = {
        coeffs.foldLeft(AnySolution: Solution[Int])({
          case (AnySolution, (a, b, c)) =>
            solveInt(a, b, c)
          case (Solutions(sols), (a, b, c)) =>
            solveInt(a, b, c) match {
              case Solutions(sols2) =>
                Solutions(sols intersect sols2)
              case AnySolution =>
                Solutions(sols)
            }
        })
      }
    }

    /*
    t=0 (p, v, a)
    t=1 (p+v+a, v+a, a)
    t=2 (p+v+a+v+a+a, v+a+a, a) == (p+2v+3a, v+2a, a)
    t=3 (p+v+a+v+a+a+v+a+a+a, v+a+a+a, a) == (p+3v+6a, v+3a, a)
    t=4 (p+v+a+v+a+a+v+a+a+a+v+a+a+a+a, v+a+a+a+a, a) == (p+4v+10a, v+4a, a)

    p + t*v + (t*(t+1)/2)*a = p + t*v + ((t*t+t)/2)*a = p + t*v + (t*t/2)*a + (t/2)*a = p + t*v + t*(a/2) + t*t*(a/2) = p + t*(v + a/2) + t*t*(a/2) =
     */

    implicit class ParticleCollides(particle: Particle) {
      private val Particle(p, v, a) = particle

      def collides(that: Particle): Quadratic.Solution[Int] = Quadratic.solveSimInt(Seq(
        ((a.x - that.a.x) / 2.0, (v.x + a.x / 2.0) - (that.v.x + that.a.x / 2.0), p.x - that.p.x),
        ((a.y - that.a.y) / 2.0, (v.y + a.y / 2.0) - (that.v.y + that.a.y / 2.0), p.y - that.p.y),
        ((a.z - that.a.z) / 2.0, (v.z + a.z / 2.0) - (that.v.z + that.a.z / 2.0), p.z - that.p.z)
      ))
    }

    override def staysClosest(particles: Seq[Particle]): Int =
      particles.zipWithIndex.minBy({ case (pos, i) =>
        (pos.a manhattanDistance Pos3(0, 0, 0), pos.v manhattanDistance Pos3(0, 0, 0), pos.p manhattanDistance Pos3(0, 0, 0)) // is this asymptote even correct?
      })._2

    override def particlesLeft(particles: Seq[Particle]): Int = {
      import Quadratic._

      def helper(collisions: Seq[(Int, Int, Int)], ps: Set[Int]): Set[Int] = collisions match {
        case Seq() => ps
        case (i, j, t) +: rest =>
          val tSet = collisions.filter(_._3 == t).flatMap(p => Seq(p._1, p._2)).toSet
          helper(rest.filterNot(p => tSet.contains(p._1) || tSet.contains(p._2)), ps -- tSet)
      }

      val collisions = particles.zipWithIndex.combinations(2).map({ case Seq((p, i), (q, j)) =>
        (i, j, p collides q)
      }).flatMap({
        case (i, j, AnySolution) =>
          Some((i, j, 0))
        case (i, j, Solutions(sols)) =>
          if (sols.nonEmpty)
            Some((i, j, sols.min))
          else
            None
      }).toSeq.sortBy(_._3)

      println(collisions)

      helper(collisions, particles.indices.toSet).size
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import QuadraticSolution._

    println(staysClosest(input))
    println(particlesLeft(input))
  }
}
