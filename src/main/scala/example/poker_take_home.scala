package test.poker

object Poker extends App {

  /*
   * Given a set of 5 playing card identifiers such as 2H, 7C, QS, 10D, 2D;
   * determine if this hand is better than some other hand, according to the rules of poker.
   *
   * Hands will be a string with 5 cards comma separated,
   * each card will have 1-2 digits or JQKA and a suit indicator C,D,S,H (i.e. 10C, KH)
   *
   * Possible Hand Types Below:
   *   Straight flush
   *   Four of a kind
   *   Full house
   *   Flush
   *   Straight
   *   Three of a kind
   *   Two pair
   *   One pair
   *
   * The goal of this is to compare between the hand types.
   * Comparing 2 of the same type (i.e. 2 straights) to determine a winner is outside the scope
   * and will not be tested.
   *
   * Implement hand1WinsOverHand2 method and return whether or not the first hand wins over the second hand.
   */

  abstract class HandType(handRank: Int) {
    def getHandRank: Int = handRank
  }

  case object StraightFlush extends HandType(8)
  case object FourOfAKind extends HandType(7)
  case object FullHouse extends HandType(6)
  case object Flush extends HandType(5)
  case object Straight extends HandType(4)
  case object ThreeOfAKind extends HandType(3)
  case object TwoPair extends HandType(2)
  case object OnePair extends HandType(1)
  case object NothingSpecial extends HandType(0)

  case class CardRank(rank: String) { 

    def toInt: Int = rank match {
      case "A" => 14
      case "K" => 13
      case "Q" => 12
      case "J" => 11
      case num => scala.util.Try(num.toInt).toOption getOrElse 0
    }
  }

  case class Card(suite: Char, rank: CardRank) {
    def this(card: String) = this(card.last, CardRank(card.init))
  }

  case class Hand(hand: Seq[Card]) {

    def isFourOfAKind: Boolean = hand
      .groupBy(_.rank.toInt)
      .filter { case (k, v) => v.length == 4 }
      .size == 1

    def isFullHouse: Boolean = hand
      .groupBy(_.rank.toInt)
      .map(_._2.length)
      .forall(v => v == 2 || v == 3)

    def isFlush: Boolean = hand.map(_.suite).distinct.length == 1

    def isStraight: Boolean = {
      val sortedHand = hand.map(_.rank.toInt)
      sortedHand.max - sortedHand.min == 4 && sortedHand.distinct.length == 5
    }

    def isThreeOfAKind: Boolean = hand
      .groupBy(_.rank.toInt)
      .filter { case (k, v) => v.length == 3 }
      .size == 1

    def isTwoPair: Boolean = hand
      .groupBy(_.rank.toInt)
      .filter { case (k, v) => v.length == 2 }
      .size == 2

    def isOnePair: Boolean = hand
      .groupBy(_.rank.toInt)
      .filter { case (k, v) => v.length == 2 }
      .size == 1

    def getHighCard: Card = hand.maxBy(_.rank.toInt)

    def getHandType: HandType = hand match {
      case _ if isStraight && isFlush => StraightFlush
      case _ if isFourOfAKind => FourOfAKind
      case _ if isFullHouse => FullHouse
      case _ if isFlush => Flush
      case _ if isStraight => Straight
      case _ if isThreeOfAKind => ThreeOfAKind
      case _ if isTwoPair => TwoPair
      case _ if isOnePair => OnePair
      case _ => NothingSpecial
    }
  }

  def hand1WinsOverHand2(hand1Str: String, hand2Str: String): Boolean = {
    val hand1 = Hand(hand1Str.split(",").map(new Card(_)))
    val hand2 = Hand(hand2Str.split(",").map(new Card(_)))

    hand1.getHandType.getHandRank > hand2.getHandType.getHandRank
  }

  implicit class CompareTwoPokerHands(hand1: String) {
    def winsOver(hand2: String): Unit = {
      val result = 
        if (hand1WinsOverHand2(hand1, hand2)) "Correct" else "Incorrect"
      println(s"$result, hand [$hand1] wins over [$hand2]")
    }
  }

  println("Poker Hand comparison")
  "8C,9C,10C,JC,QC" winsOver "6S,7H,8D,9H,10D" // straight flush
  "4H,4D,4C,4S,JS" winsOver "6C,6S,KH,AS,AD" // four of a kind
  "5C,3C,10C,KC,7C" winsOver "6C,6D,6H,9C,KD" // flush
  "4H,4D,4C,KC,KD" winsOver "9D,6S,KH,AS,AD" // full house
  "2C,3C,4S,5S,6S" winsOver "6C,6D,6H,9C,KD" // straight
  "7C,7D,7S,3H,4D" winsOver "9S,6S,10D,AS,AD" // three of a kind
  "8C,8H,10S,KH,KS" winsOver "2S,2D,JH,7S,AC" // two pair
  "AC,AH,3C,QH,10C" winsOver "3S,2D,KH,JS,AD" // one pair

  // System.exit(0)
}