package core.game.cards

enum Height(
    s: String,
    baseRank: Int,
    basePoints: Int,
    trumpRank: Int,
    trumpPoints: Int
):
  def getLiteral: String = s
  def getBaseRank: Int = baseRank
  def getTrumpRank: Int = trumpRank
  def getBasePoints: Int = basePoints
  def getTrumpPoints: Int = trumpPoints

  case Seven extends Height("7", 0, 0, 0, 0)
  case Eight extends Height("8", 1, 0, 1, 0)
  case Nine extends Height("9", 2, 0, 6, 14)
  case Ten extends Height("T", 6, 10, 4, 10)
  case Jack extends Height("J", 3, 2, 7, 20)
  case Queen extends Height("Q", 4, 3, 2, 3)
  case King extends Height("K", 5, 4, 3, 4)
  case Ace extends Height("A", 7, 11, 5, 11)
