package core.game.cards

enum RangAtout(r: Int, pts: Int) {
  case Sept extends RangAtout(0, 0)
  case Huit extends RangAtout(1, 0)
  case Reine extends RangAtout(2, 3)
  case Roi extends RangAtout(3, 4)
  case Dix extends RangAtout(4, 10)
  case As extends RangAtout(5, 11)
  case Neuf extends RangAtout(6, 14)
  case Valet extends RangAtout(7, 20)
}
