package core.game.cards

enum Suit(s: String):
  def getLiteral: String = s
  case Clubs extends Suit("c")
  case Diamonds extends Suit("d")
  case Hearts extends Suit("h")
  case Spades extends Suit("s")
  case None extends Suit("NONE")
