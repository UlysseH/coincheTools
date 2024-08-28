package core.game.cards

enum Couleur(s: String):
  def getLiteral: String = s
  case Carreau extends Couleur("d")
  case Coeur extends Couleur("h")
  case Pique extends Couleur("s")
  case Trefle extends Couleur("c")
