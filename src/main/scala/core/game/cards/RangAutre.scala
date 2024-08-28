package core.game.cards

enum RangAutre(r: Int, pts: Int) {
  case Sept extends RangAutre(0, 0)
  case Huit extends RangAutre(1, 0)
  case Neuf extends RangAutre(2, 0)
  case Valet extends RangAutre(3, 2)
  case Reine extends RangAutre(4, 3)
  case Roi extends RangAutre(5, 4)
  case Dix extends RangAutre(6, 10)
  case As extends RangAutre(7, 11)
}