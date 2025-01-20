package core.game

enum Player(s: String):
  def getLiteral: String = s

  case player1 extends Player("p1")
  case player2 extends Player("p2")
  case player3 extends Player("p3")
  case player4 extends Player("p4")
  case EMPTY extends Player("EMPTY")

  def nextPlayer: Player = s match
    case "p1" => player2
    case "p2" => player3
    case "p3" => player4
    case "p4" => player1

  def partner: Player = s match
    case "p1" => player3
    case "p2" => player4
    case "p3" => player1
    case "p4" => player2

object Player {
  def fromString(s: String): Player = s match
    case "p1" => player1
    case "p2" => player2
    case "p3" => player3
    case "p4" => player4
}

enum Team:
  case teamA
  case teamB
