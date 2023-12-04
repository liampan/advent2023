package solutions.day3

import utils.InputReader

object DayThree extends App {

  val input = InputReader.getInput("day3/input.txt")
  val test: Seq[String] ="467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..".split("\n")

  val pan = "820........782.347...................*838..*371..475.......607...\n...*..796...........449..138......991.....2........*....28....*..".split("\n")

  case class Symbol(char: Char, index: (Int, Int))

  // (-1, -1) | (-1, 0) | (-1, +1) | (-1, +2)
  // ( 0, -1) | ( 0, 0) | ( 0, +1) | ( 0, +2)
  // (+1, -1) | (+1, 0) | (+1, +1) | (+1, +2)
  case class SchematicPiece(number: Int, index: (Int, Int)) {
    def boarders(maxLine: Int, maxWidth: Int) = {
      val numLength = number.toString.length
      val lineNumber = index._1
      val firstNumIndex = index._2
      val lastNumIndex = firstNumIndex + numLength
      val charNumbers = index._2 until index._2 + numLength
      val leftBoarder = Seq(
        (lineNumber-1, firstNumIndex-1),
        (lineNumber, firstNumIndex-1),
        (lineNumber+1, firstNumIndex-1)
      )
      val rightBoard = Seq(
        (lineNumber - 1, lastNumIndex),
        (lineNumber, lastNumIndex),
        (lineNumber + 1, lastNumIndex)
      )
      val topsAndBottoms = charNumbers.flatMap(i =>
        Seq((lineNumber -1, i),(lineNumber +1, i))
      )
      (leftBoarder ++ topsAndBottoms ++ rightBoard)
        .filter{case (line, char) =>
          line >= 0 && line <= maxLine &&
            char >= 0 && char <= maxWidth
        }
    }
    def symbols(schematic: Seq[String]): Seq[Symbol] = {
      val nonSymbols = Set('.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
      boarders(schematic.length -1, schematic.head.length-1).collect {
        case (line, char) if !nonSymbols.contains(schematic(line)(char)) =>
          Symbol(schematic(line)(char), (line, char))
      }
    }
  }

  def readInput(s: Seq[String]): Seq[SchematicPiece] =
    s.zipWithIndex.foldLeft(Seq.empty[SchematicPiece]){
      case (acc, (line, lineIndex)) =>
        acc ++ line.split("""(\D+)""")
          .foldLeft(Seq.empty[SchematicPiece]) {
            case (acc, "") => acc
            case (acc, num) =>
              val index = line.indexOf(num, acc.lastOption.fold(0)(g => g.index._2+g.number.toString.length))
              acc :+ SchematicPiece(num.toInt, (lineIndex, index))
          }
  }

  def filterNonAdjacentSymbols(schema: Seq[String]): Seq[SchematicPiece] =
    readInput(schema).filter(_.symbols(schema).nonEmpty)

  def printer(schema: Seq[String]) = {
    val nums = filterNonAdjacentSymbols(schema)
    schema.zipWithIndex.foreach{ case (line, lineIndex) =>
      line.zipWithIndex.foreach{ case (char, charIndex) =>
        if (nums.exists{n =>
          val blah = (n.index._2 until n.index._2+n.number.toString.length).map((n.index._1, _))
          blah.contains((lineIndex, charIndex))}) print(Console.RED)
        if ( nums.exists { n =>
          val nonSymbols = Set('.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
          n.boarders(schema.length -1, schema.head.length-1).contains((lineIndex, charIndex)) && !nonSymbols.contains(char)
          }) print(Console.GREEN)
        print(char)
        print(Console.RESET)
      }
      println()
    }
    println(nums.map(_.number).sum)
  }

  printer(test)

  def findGears(schema: Seq[String]) = {
    readInput(schema)
      .filter{ num =>num.symbols(schema).exists(_.char == '*')}
      .groupBy(_.symbols(schema)).filter(_._2.length > 1)
      .map(_._2.map(_.number).product)
      .sum
  }

  println(findGears(input))

}
