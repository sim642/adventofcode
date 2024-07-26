package eu.sim642.adventofcode2023

object Day15 {

  def hash(s: String): Int = s.foldLeft(0)((acc, c) => (17 * (acc + c.toInt)) % 256)

  def sumStepHashes(steps: Seq[String]): Int = steps.map(hash).sum

  case class HashMap(boxes: Vector[Vector[(String, Int)]]) {
    def +(keyValue: (String, Int)): HashMap = {
      val key -> value = keyValue
      val boxI = hash(key)
      val box = boxes(boxI)
      val keyI = box.indexWhere(_._1 == key)
      val newBox =
        if (keyI < 0)
          box :+ keyValue
        else
          box.updated(keyI, keyValue)
      HashMap(boxes.updated(boxI, newBox))
    }

    def -(key: String): HashMap = {
      val boxI = hash(key)
      HashMap(boxes.updated(boxI, boxes(boxI).filterNot(_._1 == key)))
    }

    def focusingPower: Int = {
      (for {
        (box, boxI) <- boxes.zipWithIndex
        ((_, focalLength), slotI) <- box.zipWithIndex
      } yield (boxI + 1) * (slotI + 1) * focalLength).sum
    }
  }

  object HashMap {
    def empty: HashMap = HashMap(Vector.fill(256)(Vector.empty))
  }

  def installLenses(steps: Seq[String]): HashMap = {
    steps.foldLeft(HashMap.empty)({
      case (acc, s"$key=$value") => acc + (key -> value.toInt)
      case (acc, s"$key-") => acc - key
      case (_, _) => ???
    })
  }

  def focusingPower(steps: Seq[String]): Int = installLenses(steps).focusingPower


  def parseSteps(input: String): Seq[String] = input.split(',').toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumStepHashes(parseSteps(input)))
    println(focusingPower(parseSteps(input)))
  }
}
