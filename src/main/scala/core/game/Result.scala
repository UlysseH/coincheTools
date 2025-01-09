package core.game

import core.game.cards.{Card, HandMap, Suit}
import io.circe.Json
import io.circe.syntax.*

case class Result(
    startingHandMap: HandMap,
    trumpSuit: Suit,
    pointsA: Int,
    pointsB: Int,
    cardsPlayedOrdered: List[Card]
) {
  def toResultStr = ResultStr(
    startingHandMap.cards1.map(_.getNotation).mkString(","),
    startingHandMap.cards2.map(_.getNotation).mkString(","),
    startingHandMap.cards3.map(_.getNotation).mkString(","),
    startingHandMap.cards4.map(_.getNotation).mkString(","),
    trumpSuit.getLiteral,
    pointsA.toString,
    pointsB.toString,
    cardsPlayedOrdered
  )
  def toMap: Map[String, String] = Map(
    "cards1" -> startingHandMap.cards1.map(_.getNotation).mkString(","),
    "cards2" -> startingHandMap.cards2.map(_.getNotation).mkString(","),
    "cards3" -> startingHandMap.cards3.map(_.getNotation).mkString(","),
    "cards4" -> startingHandMap.cards4.map(_.getNotation).mkString(","),
    "trumpSuit" -> trumpSuit.getLiteral,
    "pointsA" -> pointsA.toString,
    "pointsB" -> pointsB.toString,
    "cardsPlayedOrdered" -> cardsPlayedOrdered.map(_.getNotation).mkString(",")
  )

  def toJson: Json = toMap.asJson
}

case class ResultStr(
    cards1: String,
    cards2: String,
    cards3: String,
    cards4: String,
    trumpSuit: String,
    pointsA: String,
    pointsB: String,
    cardsPlayedOrdered: String
)
