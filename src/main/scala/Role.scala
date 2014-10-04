import model.ActionType.{Strike, TakePuck}
import model.{Move, Game, World, Hockeyist}
import WorldEx._

trait Role {
  def move(self: Hockeyist, world: World, game: Game, move: Move): Unit

  def name: String = this.getClass.getName.split('$')(1)

  var lastStatus: String = ""

  val appender = (s: String) => lastStatus += s + "\n"

  def takeIfCan(self: Hockeyist, move: Move) = if (self.canOwnPuck) {
    lastStatus += "take\n"
    move.action = TakePuck
    true
  }
  else {
    false
  }

  def strikeEnemyIfCan(self: Hockeyist, move: Move) = {
    val enemy = world.hockeyists.filter(_.isMoveableEnemy).find(self.canPunch)
    if (enemy.isDefined) {
      move.action = Strike
    }
    enemy.isDefined
  }

  def strikeOrTakeIfCan(self: Hockeyist, move: Move) = if (self.canOwnPuck) {
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
