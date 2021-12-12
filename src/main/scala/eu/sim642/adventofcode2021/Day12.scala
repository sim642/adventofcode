package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.IteratorImplicits.*
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}

object Day12 {

  type Cave = String
  type CaveMap = Map[Cave, Set[Cave]]

  sealed trait Part {
    def countPaths(caveMap: CaveMap): Int
  }

  object Part1 extends Part {

    override def countPaths(caveMap: CaveMap): Int = {

      case class Node(path: List[Cave])

      val graphTraversal = new GraphTraversal[Node] with UnitNeighbors[Node] {
        override val startNode: Node = Node(List("start"))

        override def unitNeighbors(node: Node): IterableOnce[Node] = {
          for {
            neighbor <- caveMap(node.path.head).iterator
            if neighbor.forall(_.isUpper) || !node.path.contains(neighbor)
          } yield Node(neighbor :: node.path)
        }
      }

      val nodes = BFS.traverse(graphTraversal).nodes
      nodes.count(_.path.head == "end")
    }
  }

  object Part2 extends Part {

    override def countPaths(caveMap: CaveMap): Int = {

      case class Node(path: List[Cave], canDuplicateSmall: Boolean)

      val graphTraversal = new GraphTraversal[Node] with UnitNeighbors[Node] {
        override val startNode: Node = Node(List("start"), true)

        override def unitNeighbors(node: Node): IterableOnce[Node] = {
          if (node.path.head == "end")
            Iterator.empty
          else {
            for {
              neighbor <- caveMap(node.path.head).iterator
              if neighbor != "start"
              bigOrDuplicate = neighbor.forall(_.isUpper) || !node.path.contains(neighbor)
              if node.canDuplicateSmall || bigOrDuplicate
            } yield Node(neighbor :: node.path, node.canDuplicateSmall && bigOrDuplicate)
          }
        }
      }

      val nodes = BFS.traverse(graphTraversal).nodes
      nodes.count(_.path.head == "end")
    }
  }


  def parseCaveMap(input: String): CaveMap = {
    (for {
      line <- input.linesIterator
      Seq(x, y) = line.split("-", 2).toSeq
      p <- Seq(x -> y, y -> x)
    } yield p).groupMapReduce(_._1)(p => Set(p._2))(_ ++ _)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countPaths(parseCaveMap(input)))
    println(Part2.countPaths(parseCaveMap(input)))
  }
}
