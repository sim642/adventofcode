package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day6.FloydSolution
import eu.sim642.adventofcode2018.Day16.{Instruction, Registers}

import scala.collection.mutable
import scala.math.Integral.Implicits._


object Day21 {

  def runProgram(program: Vector[Instruction], ipRegister: Int, register0: Int): Int = {
    var registers = Seq.fill(6)(0).updated(0, register0)
    var ip = 0
    while (program.indices.contains(ip)) {
      val beforeRegisters = registers.updated(ipRegister, ip)
      val instruction = program(ip)
      val afterRegisters = instruction(beforeRegisters)
      println(s"$beforeRegisters $instruction $afterRegisters")
      ip = afterRegisters(ipRegister)
      registers = afterRegisters
      ip += 1
    }
    registers(0)
  }



  def runProgram(input: String, register0: Int = 0): Int = {
    val (ipRegister, program) = Day19.parseInput(input)
    runProgram(program, ipRegister, register0)
  }

  def detectLoop(input: String, register0: Int = 0): Boolean = {
    val (ipRegister, program) = Day19.parseInput(input)

    val prevr2: mutable.Set[Int] = mutable.Set.empty

    var registers = Seq.fill(6)(0).updated(0, register0)
    var ip = 0
    while (program.indices.contains(ip)) {
      val beforeRegisters = registers.updated(ipRegister, ip)
      if (ip == 28) {
        val r2 = beforeRegisters(2)
        println(r2)
        if (!prevr2.add(r2))
          ???
      }

      val instruction = program(ip)
      val afterRegisters = instruction(beforeRegisters)
      //println(s"$beforeRegisters $instruction $afterRegisters")
      ip = afterRegisters(ipRegister)
      registers = afterRegisters
      ip += 1
    }
    true
  }

  def reverseEngineered(r0: Int): Unit = {
    var r2s: mutable.Map[Int, Int] = mutable.Map.empty

    var r1 = 0
    var r2 = 0
    var r3 = 0
    var r4 = 0
    var r5 = 0

    r2 = 0
    var i = 0
    do {
      r5 = r2 | 65536
      r2 = 2238642
      do {
        r2 += r5 & 255
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
        r5 /= 256
      } while (256 <= r5)

      i += 1

      r2s.put(r2, i) match {
        case Some(prevr2i) =>
          println(r2s)

          println(r2s.toSeq.sortBy(_._2))
          println(s"$r2 $i $prevr2i")
          return
        case None =>
      }
    } while (r2 != r0)
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    //runProgram(input, 0)
    detectLoop(input, 0)
    //reverseEngineered(0)

    // 6401 - too low
    // 3198114 - too low
    // part 1 - 13970209
  }
}
