package eu.sim642.adventofcode2017

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

  def parseComponents(input: String): Components = input.linesIterator.map(parseComponent).toSet

  type Bridge = Seq[Component]

  implicit class BridgeOps(bridge: Bridge) {
    def strength: Int = bridge.map(_.strength).sum
  }

  def validBridges(components: Components): Iterator[Bridge] = {

    def helper(components: Components, port: Int): Iterator[Bridge] = {
      val portComponents = components.filter(_.contains(port))
      for {
        component <- portComponents.iterator
        newPort = component.other(port)
        bridge <- Iterator.single(Nil) ++ helper(components - component, newPort)
      } yield component +: bridge
    }

    helper(components, 0)
  }

  def validLongBridges(components: Components): Iterator[Bridge] = {

    def helper(components: Components, port: Int): Iterator[Bridge] = {
      val portComponents = components.filter(_.contains(port))
      if (portComponents.isEmpty)
        Iterator.single(Nil)
      else
        for {
          component <- portComponents.iterator
          newPort = component.other(port)
          bridge <- helper(components - component, newPort)
        } yield component +: bridge
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

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(strongestBridgeStrength(input))
    println(longestBridgeStrength(input))
  }
}
