import model.ActionType.{Strike, TakePuck}
import model.{Move, Game, World, Hockeyist}
import WorldEx._

trait Role {
  def move(self: Hockeyist, world: World, game: Game, move: Move): Unit

  def name: String = this.getClass.getName.split('$')(1)

  var lastStatus: String = ""

  def ownPuckIfCan(self: Hockeyist, move: Move) = if (self.canOwnPuck) {
    if (Roles.puckWillGoToOurNetAfterStrike(self)) {
      lastStatus += "\ntake!"
      move.action = TakePuck
    }
    else {
      lastStatus += "\nstrike!"
      move.action = Strike
    }
    true
  } else false


}
