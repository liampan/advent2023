package solutions.day4

import utils.InputReader

object DayFour extends App {

  val input = InputReader.getInput("day4/input.txt")
  val test: Seq[String] ="Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11".split("\n")

  case class Card(number: Int, numberEntries: Set[Int], winningNumbers: Set[Int]){
    private def numberOfWins: Int = numberEntries.intersect(winningNumbers).size
    def points: Int = if (numberOfWins == 0) 0
      else (1 until numberOfWins).foldLeft(1)((a, _) =>a *2)

    private def cardsWon: Range.Inclusive = number+1 to number+numberOfWins

    def collectWinnings(all: Seq[Card]): Int = {
      cardsWon.size + cardsWon
        .map(c => all.find(_.number == c).get.collectWinnings(all))
        .sum
    }
  }

  object Card {
    private val cardRegex = """Card +(\d+):([\d ]*)\|([\d ]*)""".r
    def apply(s: String): Card = {
      def pluck(s: String) = s.trim.split(" ").collect { case i if i.nonEmpty => i.toInt }.toSet
      s match {
        case cardRegex(a, b, c) =>
          new Card(a.toInt, pluck(b), pluck(c))
      }
    }
  }

  def partTwo(seq: Seq[Card]) = {
    seq.map(_.collectWinnings(seq)).sum + seq.size
  }

  println(partTwo(input.map(Card.apply)))
}
