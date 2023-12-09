package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2018.Day16.{Instruction, Registers}
import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder

import scala.util.control.Breaks._

object Day21 {

  trait Solution {
    def iterater2(input: String): Iterator[Int]

    def firstHaltr0(input: String): Int = iterater2(input).head

    def lastHaltr0(input: String): Int = {
      NaiveCycleFinder.find(iterater2(input)).get.cycleLast
    }
  }

  object SimulateSolution extends Solution {
    def iterateSimulation(program: Vector[Instruction], ipRegister: Int): Iterator[Registers] = {
      Iterator.iterate(Seq.fill(6)(0))({ beforeRegisters =>
        val ip = beforeRegisters(ipRegister)
        assert(program.indices.contains(ip), "program does not halt")
        val instruction = program(ip)
        val afterRegisters = instruction(beforeRegisters)
        afterRegisters.updated(ipRegister, afterRegisters(ipRegister) + 1)
      })
    }

    override def iterater2(input: String): Iterator[Int] = {
      val (ipRegister, program) = Day19.parseInput(input)
      iterateSimulation(program, ipRegister).filter(_(ipRegister) == 28).map(_(2))
    }
  }

  object ReverseEngineeredSolution extends Solution {
    override def iterater2(input: String): Iterator[Int] = {
      // ignores input...

      Iterator.iterate(Seq.fill(6)(0))({ beforeRegisters =>
        var Seq(r0, r1, r2, r3, r4, r5) = beforeRegisters

        r5 = r2 | 65536
        r2 = 2238642
        breakable {
          while (true) {
            r3 = r5 & 255
            r2 += r3
            r2 &= 16777215
            r2 *= 65899
            r2 &= 16777215

            /*if (256 <= r5) {
              // divide r5 by 256, i.e. right shift by 8
              r3 = 0
              do {
                r1 = r3 + 1
                r1 *= 256
                if (r1 <= r5)
                  r3 += 1
              } while (r1 <= r5)
              r5 = r3
            }*/
            if (256 <= r5) {
              r5 /= 256
            }
            else
              break()
          }
        }

        Seq(r0, r1, r2, r3, r4, r5)
      }).drop(1).map(_(2))
    }
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import ReverseEngineeredSolution._
    println(firstHaltr0(input))
    println(lastHaltr0(input))

    // 6401 - too low
    // 3198114 - too low
    // part 1 - 13970209, first r2
    // part 2 - 6267260, last r2 before duplicate
  }
}
