package solutions.day7

import utils.InputReader


object DaySeven_part2 extends App {
  val input = InputReader.getInput("day7/input.txt")
  val test = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483".split("\n")
  val cardOrder = List('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J')

  def cardStrength(card: Char): Int = cardOrder.reverse.indexOf(card)

  case class Hand(cards: Seq[Char], _type: HandType, bid: Int) extends Ordered[Hand] {

    implicit def orderingByCardOrder: Ordering[Char] = Ordering.by(cardStrength)

    override def compare(that: Hand): Int = {
      val primary = this._type.strength.compare(that._type.strength)
      if(primary != 0) primary
      else {
        val compareable = this.cards.map(cardStrength).zip(
          that.cards.map(cardStrength)
        ).find(a => a._1 != a._2)
        compareable.fold(0){case (_this, _that) =>
          if (_this > _that) 1 else -1
        }
      }
    }
  }
  abstract class HandType(val strength: Int)
  case object FiveOfAKind extends HandType(7) // 1 distinct cards
  case object FourOfAKind extends HandType(6) // 2 distinct cards and 4 match
  case object FullHouse extends HandType(5) // 2 distinct cards
  case object ThreeOfAKind extends HandType(4) // 3 distinct cards and 3 match
  case object TwoPair extends HandType(3) // 3 distinct cards
  case object OnePair extends HandType(2) //4 distinct cards
  case object HighCard extends HandType(1) // 5 distinct cards

  def parse(s: String) = {
    val cards :: bid :: Nil = s.split(" ").toList

    val _type = {cardOrder.map{replace =>
        val replacedCards = cards.replace('J', replace)
        val grouped = replacedCards.toList.groupBy(c => replacedCards.count(c == _))
        grouped.toList match {
          case List(5 -> _) => FiveOfAKind
          case List(1 -> _, 4 -> _) | List(4 -> _, 1 -> _) => FourOfAKind
          case List(2 -> _, 3 -> _) | List(3 -> _, 2 -> _) => FullHouse
          case List(1 -> _, 3 -> _) | List(3 -> _, 1 -> _) => ThreeOfAKind
          case List(1 -> List(_), 2 -> _) | List(2 -> _, 1 -> List(_)) => TwoPair
          case List(1 -> _, 2 -> _) | List(2 -> _, 1 -> _) => OnePair
          case _ => HighCard
        }
      }.maxBy(_.strength)
    }


    Hand(cards = cards, _type = _type, bid = bid.toInt)
  }

  println(input.map(parse).sorted.zipWithIndex.map{ case (hand, i) => hand.bid * (i+1)}.sum)

}

