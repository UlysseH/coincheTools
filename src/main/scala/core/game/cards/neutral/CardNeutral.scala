package core.game.cards.neutral

import core.game.cards.Height

case class CardNeutral(suitNeutral: SuitNeutral, height: Height) {
  def getNotation: String = s"${height.getLiteral}${suitNeutral.getLiteral}"
}

object CardNeutral {
  def fromLitteral(s: String) = CardNeutral(
    SuitNeutral.fromString(s(1).toString),
    Height.fromString(s(0).toString)
  )
}
