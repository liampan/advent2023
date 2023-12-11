package solutions.day8

import utils.InputReader

import scala.annotation.tailrec


object DayEight extends App {

  val input = InputReader.getInput("day8/input.txt")
  val test1 = "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)".split("\n")
  val test2 = "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)".split("\n")

  case class Node(key: String, left: String, right: String) {
    def next(lr: Char): String = {
      lr match {
        case 'L' => left
        case 'R' => right
        case _ => throw new Exception(s"bad input for L/R: [$lr]")
      }
    }
  }

 case class NetworkMap(leftRights:  Seq[Char], nodes: Seq[Node]) {
   def getNode(key: String) = nodes.find(_.key == key).get
   def navigateToZZZ: Int = {
     val startingNode = getNode("AAA")
     @tailrec
     def loop(node: Node, step: Int): Int= {
       if (node.key == "ZZZ") step
       else {
         val lr = leftRights(step % leftRights.length)
         loop(getNode(node.next(lr)), step + 1)
       }
     }
     loop(startingNode, 0)
   }

   def navigateAtoZ: BigInt = {
     val startingNodes: List[Node] = nodes.filter(_.key.endsWith("A")).toList

     @tailrec
     def loop(node: Node, step: Int): Int = {
       if (node.key.endsWith("Z")) step
       else {
         val lr = leftRights(step % leftRights.length)
         loop(getNode(node.next(lr)), step + 1)
       }
     }

     val steps = startingNodes.map { node =>
       loop(node, 0)
     }
     println(steps)
     lcm(steps.map(BigInt(_)))
   }
 }

  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)
  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(BigInt(1))((a, b) => (a / gcd(a, b)) * b)

  object NetworkMap {
    def apply(seq: Seq[String]): NetworkMap = {
      val leftRights = seq.head
      val regex = """([\w]{3}) = \(([\w]{3}), ([\w]{3})\)""".r

      val nodes = seq.drop(2).map {
        case regex(key, left, right) => Node(key, left, right)
      }
      NetworkMap(leftRights, nodes)
    }
  }

  println(NetworkMap(input).navigateAtoZ)



}

