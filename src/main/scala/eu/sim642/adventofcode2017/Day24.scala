package eu.sim642.adventofcode2017

import scala.collection.AbstractIterator

object Day24 {

  case class Component(a: Int, b: Int) {
    def contains(i: Int): Boolean = a == i || b == i

    def other(i: Int): Int = if (a == i) b else a

    def strength: Int = a + b
  }

  private val componentRegex = """(\d+)/(\d+)""".r

  def parseComponent(str: String): Component = str match {
    case componentRegex(a, b) => Component(a.toInt, b.toInt)
  }

  type Components = Set[Component]

  def parseComponents(input: String): Components = input.lines.map(parseComponent).toSet

  type Bridge = Seq[Component]

  implicit class BridgeOps(bridge: Bridge) {
    def strength: Int = bridge.map(_.strength).sum
  }

  // Scala 2.13 regression workaround: https://github.com/scala/bug/issues/11630
  implicit class OldIteratorFlatMap[A](it: Iterator[A]) {
    def oldFlatMap[B](f: A => IterableOnce[B]): Iterator[B] = new AbstractIterator[B] {
      private var cur: Iterator[B] = Iterator.empty
      private def nextCur() { cur = f(it.next()).iterator }
      def hasNext: Boolean = {
        // Equivalent to cur.hasNext || self.hasNext && { nextCur(); hasNext }
        // but slightly shorter bytecode (better JVM inlining!)
        while (!cur.hasNext) {
          if (!it.hasNext) return false
          nextCur()
        }
        true
      }
      def next(): B = (if (hasNext) cur else Iterator.empty).next()
    }
  }

  def validBridges(components: Components): Iterator[Bridge] = {

    def helper(components: Components, port: Int): Iterator[Bridge] = {
      val portComponents = components.filter(_.contains(port))
      portComponents.iterator
        .oldFlatMap { component =>
          val newPort = component.other(port)

          (Iterator.single(Nil) ++ helper(components - component, newPort))
            .map(bridge => component +: bridge)
        }
    }

    helper(components, 0)
  }

  def validLongBridges(components: Components): Iterator[Bridge] = {

    def helper(components: Components, port: Int): Iterator[Bridge] = {
      val portComponents = components.filter(_.contains(port))
      if (portComponents.isEmpty)
        Iterator.single(Nil)
      else
        portComponents.iterator
          .oldFlatMap { component =>
            val newPort = component.other(port)

            helper(components - component, newPort)
              .map(bridge => component +: bridge)
          }
    }

    helper(components, 0)
  }

  def strongestBridgeStrength(components: Components): Int = validLongBridges(components).map(_.strength).max

  def strongestBridgeStrength(input: String): Int = strongestBridgeStrength(parseComponents(input))


  def validLongestBridges(components: Components): Iterator[Bridge] = {
    val bridges = validLongBridges(components).toSet
    val maxLength = bridges.map(_.length).max
    bridges.filter(_.length == maxLength).iterator
  }

  def longestBridgeStrength(components: Components): Int = validLongestBridges(components).map(_.strength).max

  def longestBridgeStrength(input: String): Int = longestBridgeStrength(parseComponents(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(strongestBridgeStrength(input))
    println(longestBridgeStrength(input))
  }
}
