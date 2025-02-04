package core.game.cards.neutral

import core.game.cards.neutral.SuitNeutral.{First, Second, Third, Trump}
import core.game.cards.{Card, Hand, Height, Suit}

import scala.annotation.tailrec

// todo: add the neutral notation mapping (to compare with other players hand
case class HandNeutral(cards: List[CardNeutral]) {
  override def toString: String = cards.map(_.toString).mkString(",")

  def pointsInHand: Int = cards
    .map(card =>
      if (card.suitNeutral.==(Trump)) card.height.getTrumpPoints
      else card.height.getBasePoints
    )
    .sum

  def trumps: HandNeutral = HandNeutral(
    cards.filter(_.suitNeutral.==(Trump))
  )
  def others: HandNeutral = HandNeutral(
    cards.filterNot(_.suitNeutral.==(Trump))
  )

  def othersX: HandNeutral = HandNeutral(
    cards.filter(_.suitNeutral.==(First))
  )

  def othersY: HandNeutral = HandNeutral(
    cards.filter(_.suitNeutral.==(Second))
  )

  def othersZ: HandNeutral = HandNeutral(
    cards.filter(_.suitNeutral.==(Third))
  )

  def sorted: HandNeutral = HandNeutral(
    cards
      .filter(_.suitNeutral.==(Trump))
      .sortBy(_.height.getTrumpRank)
      .reverse ++
      cards
        .filter(_.suitNeutral.==(First))
        .sortBy(_.height.getBaseRank)
        .reverse ++
      cards
        .filter(_.suitNeutral.==(Second))
        .sortBy(_.height.getBaseRank)
        .reverse ++
      cards.filter(_.suitNeutral.==(Third)).sortBy(_.height.getBaseRank).reverse
  )

  /** [(J, 9, A, X), (A, T, X), (X)] [(J, X, X), (A, X), (A), (X, X)] ...
    */

  /** Examples: "J9A,4+" -> "J9A87", "J9AT", etc... filterS:
    * "TrumpCards,numbertrumpCards" Or : J,A,K-,K-: J, A , two cards under or
    * equal to K
    */
//  def classifyTrumps(filterS: String) = {
//    filterS.split
//  }

}

object HandNeutral {
  def fromString(s: String): HandNeutral = {
    val cards = s.split(",").toList.map(CardNeutral.fromLitteral)
    HandNeutral(cards)
  }

  /** This internal function compare two sets of cards of non-suited colors with
    * the logic (Ax) > (Ty, Ky, 7y) > (Tz, 8z) true if first is superior, false
    * if interior
    */
  // ou alors on se fait pas trop chier
  def sortColors(cardsL: List[List[Card]]): List[List[Card]] = cardsL
    .sortBy(cards =>
      cards
        .map(_.height.getBasePoints)
        .map(i => i * i * i * i)
        .sum + cards.length
    )
    .reverse

  def fromHand(hand: Hand, trumpSuit: Suit): HandNeutral = {
    val cards = hand.cards

    val trumpCardsNeutral = cards
      .filter(_.suit.==(trumpSuit))
      .map(card => CardNeutral(Trump, card.height))
      .sortBy(_.height.getTrumpRank)
      .reverse
    val restGroupedBySuit = cards
      .filterNot(_.suit.==(trumpSuit))
      .groupBy(_.suit)
      .toList
      .map((_, cards) => cards.sortBy(_.height.getBaseRank).reverse)
    val restSorted = sortColors(restGroupedBySuit)
    /*HandNeutral(
      trumpCardsNeutral ++ restGroupedBySuit.
        .getOrElse(Nil)
        .map(card => CardNeutral(First, card.height))
    )*/
    restSorted.length match
      case 0 => HandNeutral(trumpCardsNeutral)
      case 1 =>
        HandNeutral(
          trumpCardsNeutral ++ restSorted.head.map(card =>
            CardNeutral(First, card.height)
          )
        )
      case 2 =>
        HandNeutral(
          trumpCardsNeutral ++ restSorted.head.map(card =>
            CardNeutral(First, card.height)
          ) ++ restSorted(1).map(card => CardNeutral(Second, card.height))
        )
      case 3 =>
        HandNeutral(
          trumpCardsNeutral ++ restSorted.head.map(card =>
            CardNeutral(First, card.height)
          ) ++ restSorted(1).map(card =>
            CardNeutral(Second, card.height)
          ) ++ restSorted(2).map(card => CardNeutral(Third, card.height))
        )
  }
  def fromNotation(s: String): List[List[CardNeutral]] = {
    s.split(",")
      .headOption
      .match
        case Some(head) =>
          head.length match {
            case 2 =>
              fromNotation(s.split(",").tail.mkString(","))
                .map(_.prepended(CardNeutral.fromLitteral(head.mkString(""))))
            case 3 if head(2).toString.==("-") => {
              val card = CardNeutral.fromLitteral(head.slice(0, 2))
              val res =
                if (head(1).toString.==("t")) {
                  val y = fromNotation(s.split(",").tail.mkString(","))
                    .map(l =>
                      Height.values.toList
                        .filter(_.getTrumpRank.<=(card.height.getTrumpRank))
                        .map(height => CardNeutral(Trump, height))
                        .map(card => l.prepended(card))
                    )
                    .map(
                      _.prepended(CardNeutral.fromLitteral(head.mkString("")))
                    )
                  y
                } else
                  fromNotation(s.split(",").tail.mkString(","))
                    .map(l =>
                      Height.values.toList
                        .filter(_.getTrumpRank.<=(card.height.getTrumpRank))
                        .map(height =>
                          CardNeutral(
                            SuitNeutral.fromString(head(1).toString),
                            height
                          )
                        )
                        .map(card => l.prepended(card))
                    )
                    .map(
                      _.prepended(CardNeutral.fromLitteral(head.mkString("")))
                    )
              res
            }
            case 3 if head(2).toString.==("+") => {
              val card = CardNeutral.fromLitteral(head.slice(0, 2))
              val res =
                if (head(1).toString.==("t"))
                  Height.values
                    .filter(_.getTrumpRank.>=(card.height.getTrumpRank))
                    .map(height => CardNeutral(Trump, height))
                else
                  Height.values
                    .filter(_.getBaseRank.>=(card.height.getBaseRank))
                    .map(height =>
                      CardNeutral(
                        SuitNeutral.fromString(head(1).toString),
                        height
                      )
                    )
              (res.toList ++ fromNotation(
                s.split(",").tail.mkString(",")
              )).distinct
            }
            case _ => {
              println(s)
              Nil
            }
          }
        case None => Nil
  }

  /** Ex: Jt,At,Kt-,Kt-: J, A , two cards under, at trump suit
    */
  def generateFromNotation(s: String) = s.split(",").map {
    case t if t.length.==(2) => List(CardNeutral.fromLitteral(t.mkString("")))
    case t if t.length.==(3) && t(2).toString.==("-") => {
      val card = CardNeutral.fromLitteral(t.slice(0, 1))
      val res =
        if (t(1).toString.==("t"))
          Height.values
            .filter(_.getTrumpRank.<=(card.height.getTrumpRank))
            .map(height => CardNeutral(Trump, height))
        else
          Height.values
            .filter(_.getBaseRank.<=(card.height.getBaseRank))
            .map(height =>
              CardNeutral(SuitNeutral.fromString(t(1).toString), height)
            )
      res.toList
    }
  }
}
