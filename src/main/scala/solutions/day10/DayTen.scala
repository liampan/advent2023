package solutions.day10

import utils.InputReader

import scala.collection.immutable.Seq
import scala.util.Try

object DayTen extends App {

  val input: List[String] = InputReader.getInput("day10/input.txt")
  val test: List[String] = "...........\n.S-------7.\n.|F-----7|.\n.||.....||.\n.||.....||.\n.|L-7.F-J|.\n.|..|.|..|.\n.L--J.L--J.\n...........".split("\n").toList
  val test1: List[String] = "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L".split("\n").toList

  def findRuns(s: Seq[Int]): Seq[Seq[Int]] = {
    def loop(s: Seq[Int], ss: Seq[Seq[Int]], i: Int): Seq[Seq[Int]] = {
      if (i == s.length) ss
      else {
        val next = s(i)
        ss.lastOption.fold(loop(s, Seq(Seq(next)), i + 1)) { l =>
          val newSS = if (l.last + 1 == next) ss.dropRight(1) :+ (l :+ next)
          else ss :+ Seq(next)
          loop(s, newSS, i + 1)
        }
      }
    }

    loop(s, Seq.empty, 0)
  }

  abstract class Direction(val to: (Int, Int))
  case object North extends Direction((-1, 0))
  case object South extends Direction((+1, 0))
  case object East extends Direction((0, +1))
  case object West extends Direction((0, -1))

  abstract class Pipe(val connects: (Direction, Direction)){
    val position: (Int, Int)
    def connectsToAndFrom = Seq(move(position, connects._1), move(position, connects._2))
  }
  case class Start(position: (Int, Int), override val connects: (Direction, Direction)) extends Pipe(connects)
  case class Vertical(position: (Int, Int)) extends Pipe(North, South)
  case class Horizontal(position: (Int, Int)) extends Pipe(East, West)
  case class Corner(position: (Int, Int), override val connects: (Direction, Direction)) extends Pipe(connects)

  object Pipe {
    def apply(matrix: List[String], position: (Int, Int)): Option[Pipe] = {
      Try(matrix(position._1)(position._2)).toOption.flatMap {
          case '|' => Some(Vertical(position))
          case '-' => Some(Horizontal(position))
          case 'L' => Some(Corner(position, (North, East)))
          case 'J' => Some(Corner(position, (West, North)))
          case '7' => Some(Corner(position, (West, South)))
          case 'F' => Some(Corner(position, (South, East)))
          case _ => None
      }
    }
  }

  def move(position: (Int, Int), direction: Direction) = {
    (position._1 + direction.to._1, position._2 + direction.to._2)
  }

  case class PipeSystem(start: (Int, Int), pipes: List[Pipe], matrix: List[String]) {
    def next = {
      val nextPipes = pipes.takeRight(2).distinct.flatMap { p =>
        p.connectsToAndFrom.filterNot(x => pipes.exists(_.position == x)).map(a => Pipe.apply(matrix, a).get)
      }
      copy(pipes = (pipes ++ nextPipes).distinct)
    }

    def generate: PipeSystem = {
      val n = next
      if (next == this) this
      else n.generate
    }

    // outside == even
    // inside == odd
    lazy val insideLoop = {
      matrix.zipWithIndex.flatMap { case (line, down) =>
        line.zipWithIndex.map { case (char, across) =>
          val isPartOfLoop = pipes.exists(_.position == (down, across))
          val pipesInRay = pipes.filter(p => p.position._1 == down && p.position._2 < across)
          val rayCast = pipesInRay.sortBy(_.position._2).foldLeft(Seq.empty[Seq[Pipe]]) { case (acc, pipe) =>
            if (pipe.isInstanceOf[Vertical]) acc :+ Seq(pipe)
            else {
              acc.lastOption.fold(acc :+ Seq(pipe)) { last =>
                last.last match {
                  case _: Vertical => acc :+ Seq(pipe)
                  case _: Horizontal => acc.dropRight(1) :+ (last :+ pipe)
                  case _ if last.length == 1 => acc.dropRight(1) :+ (last :+ pipe)
                  case _ => acc :+ Seq(pipe)
                }
              }
            }
          }.filterNot { pipes =>
            pipes.head.connects._1 == pipes.last.connects._2
          }
          val rayCastIsEve = rayCast.length % 2 == 0

          (char, down, across, !isPartOfLoop && !rayCastIsEve)
        }}.filter { case (char, down, across, boolean) => boolean //todo inefficent however useful for debug
      }
    }
  }

  object PipeSystem {
    def apply(matrix: List[String]): PipeSystem = {
      val down = matrix.indexWhere(_.contains('S'))
      val across = matrix(down).indexOf('S')
      val start = (down, across)
      val directions = Seq(North, South, East, West).filter { d =>
        val pos = move(start, d)
        val pipe = Pipe.apply(matrix, pos)
        pipe.fold(false) { pipe =>
          val (d1, d2) = pipe.connects
          move(pos, d1) == start || move(pos, d2) == start
        }
      }
      val startPipe = Start(start, (directions.head, directions.last))
      new PipeSystem(start, startPipe +: directions.flatMap { d =>
        val p = move(start, d)
        Pipe.apply(matrix, p)
      }.toList, matrix)
    }
  }


  def printer(matrix: List[String], pipeSystem: PipeSystem) = {
    val charMap = Map('-' -> "━", '|' -> "┃", 'L' -> "┗", 'J' -> "┛", 'F' -> "┏", '7' -> "┓").withDefault(identity)
    matrix.zipWithIndex.foreach{ case (line, down) =>
      line.zipWithIndex.foreach{ case (char, across) =>
        if (pipeSystem.pipes.exists(_.position == (down, across))) print(Console.BLUE)
        if (pipeSystem.insideLoop.exists{case (_, d, a, _) => (d, a) == (down, across)}) print(Console.RED)
        print(charMap(char))
        print(Console.RESET)
      }
      println()
    }
  }
  val use = input
  val pipeSystem = PipeSystem(use).generate
  printer(use, pipeSystem)
  println("PART ONE:" + pipeSystem.pipes.length/2)


  //https://en.wikipedia.org/wiki/Point_in_polygon#Ray_casting_algorithm
  val thing = pipeSystem.insideLoop
  println(thing.length)

}
