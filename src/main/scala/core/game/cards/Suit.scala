package core.game.cards

enum Suit(s: String):
  def getLiteral: String = s
  def generateCards: List[Card] = Height.values.map(h => Card(this, h)).toList

  case Clubs extends Suit("c")
  case Diamonds extends Suit("d")
  case Hearts extends Suit("h")
  case Spades extends Suit("s")
  case None extends Suit("NONE")

object Suit {
  def fromString(s: String) = s match
    case "c"        => Clubs
    case "h"        => Hearts
    case "s"        => Spades
    case "d"        => Diamonds
    case "Clubs"    => Clubs
    case "Hearts"   => Hearts
    case "Spades"   => Spades
    case "Diamonds" => Diamonds
}
