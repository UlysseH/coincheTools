package core.game.bidding

import core.game.Player.{player1, player3}
import core.game.Team.teamA
import core.game.{Player, Result, Team}

enum Contract(value: Int):
  def pointsMade: Int = value
  case Eighty extends Contract(80)
  case Ninety extends Contract(90)
  case Hundred extends Contract(100)
  case HundredTen extends Contract(110)
  case HundredTwenty extends Contract(120)
  case HundredThirty extends Contract(130)
  case HundredFourty extends Contract(140)
  case HundredFifty extends Contract(150)
  case HundredSixty extends Contract(160)
  case Capot extends Contract(250)

object Contract {
  def isMet(
      result: Result,
      team: Team,
      contract: Contract
  ): Boolean = {
    val points =
      if (team == teamA) result.pointsA else result.pointsB
    contract match
      case Contract.Eighty        => points >= 82
      case Contract.Ninety        => points >= 90
      case Contract.Hundred       => points >= 100
      case Contract.HundredTen    => points >= 110
      case Contract.HundredTwenty => points >= 120
      case Contract.HundredThirty => points >= 130
      case Contract.HundredFourty => points >= 140
      case Contract.HundredFifty  => points >= 150
      case Contract.HundredSixty  => points >= 160
      case Contract.Capot =>
        if (team == teamA) (result.pointsB == 0)
        else (result.pointsA == 0)
  }

  def pointsByContract(points: Int, contract: Contract): Int = contract match
    case Contract.Eighty        => if (points >= 82) 80 else -160
    case Contract.Ninety        => if (points >= 90) 90 else -160
    case Contract.Hundred       => if (points >= 100) 100 else -160
    case Contract.HundredTen    => if (points >= 110) 110 else -160
    case Contract.HundredTwenty => if (points >= 120) 120 else -160
    case Contract.HundredThirty => if (points >= 130) 130 else -160
    case Contract.HundredFourty => if (points >= 140) 140 else -160
    case Contract.HundredFifty  => if (points >= 150) 150 else -160
    case Contract.HundredSixty  => if (points >= 160) 160 else -160
    case Contract.Capot         => if (points == 162) 250 else -160

  def pointsByContractSummary(pointsL: List[Int]): List[String] =
    Contract.values.toList.map(contract =>
      s"[average] ${contract.toString}: ${pointsL
          .map(points => pointsByContract(points, contract))
          .sum / pointsL.length}"
    )

  def pointsMade(
      result: Result,
      team: Team,
      contract: Contract
  ): Int = if (isMet(result, team, contract)) contract.pointsMade else -160

  def evFromSimulations(
      results: List[Result],
      team: Team
  ): List[(Contract, Int)] =
    Contract.values.toList.map(contract => {
      val outcomes =
        results.map(res => Contract.pointsMade(res, teamA, contract))
      val ev = outcomes.sum / outcomes.length
      (contract, ev)
    })
}
