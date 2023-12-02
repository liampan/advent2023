package solutions.day2

import utils.InputReader

object DayTwo extends App {

  val input = InputReader.getInput("day2/input.txt")

  case class Game(number: Int, turns: Seq[Map[String, Int]]) {
    private def maxBy(colour: String) =turns.map(_.getOrElse(colour, 0)).max
    val maxRed: Int = maxBy("red")
    val maxGreen: Int = maxBy("green")
    val maxBlue: Int = maxBy("blue")

    val power: Int = maxRed * maxBlue * maxGreen
  }

  val test: Seq[String] = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green".split("\n")

  //Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  def parseInputString(s: String) = {
    val gameR = """Game (\d+):(.*)""".r
    val cubesR = """\s?(\d+) (\w+)""".r
    s match {
      case gameR(num, rest) =>
        val turns = rest.split(';').map(_.strip().split(',').map {
          case cubesR(amount, colour) => colour -> amount.toInt
          case x => throw new Exception(s"not parsable:[$x]")
          }.toMap
        ).toSeq
        Game(num.toInt, turns)
      case _ => throw new Exception(s"not parsable: $s")
    }
  }

  def filterForPossibleGames(s: Seq[String]) = {
    val games = s.map(parseInputString)
    games.filter{ game =>
        game.maxRed <= 12 &&
        game.maxGreen <= 13 &&
        game.maxBlue <= 14
    }
  }

  def sumGameNumberFilteredGames(s: Seq[String]) = {
    filterForPossibleGames(s).map(_.number).sum
  }

  def sumPowerGames(s: Seq[String]) = {
    val games = s.map(parseInputString)
    games.map(_.power).sum
  }



  println(sumPowerGames(input))

}
