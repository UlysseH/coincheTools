package core.game.cards.neutral

/** The idea is to provide a standard notation to regroup meaningless color
 * variance to render notions such as "9 second", "10 second"
 */
enum SuitNeutral(s: String):
  def getLitteral: String = s
  
  case Trump extends SuitNeutral("t")
  case First extends SuitNeutral("x")
  case Second extends SuitNeutral("y")
  case Third extends SuitNeutral("z")

object SuitNeutral {
  def fromString(s: String) = s match
    case "t" => Trump
    case "x" => First
    case "y" => Second
    case "z" => Third
}