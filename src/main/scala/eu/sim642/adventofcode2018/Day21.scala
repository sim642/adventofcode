package eu.sim642.adventofcode2018

import scala.collection.mutable
import scala.util.control.Breaks._

object Day21 {

  trait Solution {
    def r2Seq(input: String): Seq[Int]

    def firstHaltr0(input: String): Int = r2Seq(input).head
    def lastHaltr0(input: String): Int = r2Seq(input).last
  }

  object SimulateSolution extends Solution {
    override def r2Seq(input: String): Seq[Int] = {
      val (ipRegister, program) = Day19.parseInput(input)

      var r2s: mutable.LinkedHashSet[Int] = mutable.LinkedHashSet.empty // keeps order but allows fast contains

      var registers = Seq.fill(6)(0)
      var ip = 0
      while (program.indices.contains(ip)) {
        val beforeRegisters = registers.updated(ipRegister, ip)
        if (ip == 28) {
          val r2 = beforeRegisters(2)
          if (!r2s.add(r2))
            return r2s.toSeq
        }

        val instruction = program(ip)
        val afterRegisters = instruction(beforeRegisters)
        //println(s"$beforeRegisters $instruction $afterRegisters")
        ip = afterRegisters(ipRegister)
        registers = afterRegisters
        ip += 1
      }
      ???
    }

    override def firstHaltr0(input: String): Int = {
      // TODO: avoid duplication
      val (ipRegister, program) = Day19.parseInput(input)

      var registers = Seq.fill(6)(0)
      var ip = 0
      while (program.indices.contains(ip)) {
        val beforeRegisters = registers.updated(ipRegister, ip)
        if (ip == 28) {
          val r2 = beforeRegisters(2)
          return r2 // return immediately on first r2
        }

        val instruction = program(ip)
        val afterRegisters = instruction(beforeRegisters)
        //println(s"$beforeRegisters $instruction $afterRegisters")
        ip = afterRegisters(ipRegister)
        registers = afterRegisters
        ip += 1
      }
      ???
    }
  }

  object ReverseEngineeredSolution extends Solution {
    override def r2Seq(input: String): Seq[Int] = {
      // ignores input...

      var r2s: mutable.LinkedHashSet[Int] = mutable.LinkedHashSet.empty // keeps order but allows fast contains

      var r0 = 0
      var r1 = 0
      var r2 = 0
      var r3 = 0
      var r4 = 0
      var r5 = 0

      r2 = 0
      do {
        r5 = r2 | 65536
        r2 = 2238642
        breakable {
          do {
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
          } while (true)
        }

        //println(r2)
        if (!r2s.add(r2))
          return r2s.toSeq
      } while (r2 != r0)
      ???
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

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
