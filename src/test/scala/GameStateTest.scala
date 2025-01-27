/*import core.game.GameV2
import core.game.cards.{Card, HandMap}
import core.game.cards.Suit.Spades
import org.scalatest.funsuite.AnyFunSuite

class GameStateTest extends AnyFunSuite:
  test("Playable Cards generation" {
    val hMap = HandMap.fromStrings(
      "Js,Qs,8s,Ac,Tc,Qd,Jc,9d",
      "9s,Ks,Th,Kd,Qh,8d,7d,7h",
      "Ts,Ah,Ad,Td,Qc,Jd,9h,7c",
      "As,7s,Kh,Kc,Jh,9c,8h,8c"
    )

    val game = GameV2("aze", hMap, Spades)
    val initialGameState = game.initialGameState
    val stepOne = initialGameState.computeNextGameState(Card.fromLitteral("Ac"), true)
    
    val playableCards = stepOne.generatePlayableCards
  }*/
