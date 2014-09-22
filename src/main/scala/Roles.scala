import model.ActionType.{TakePuck, Strike}
import model.{Move, Game, World, Hockeyist}

import WorldEx._

object Roles {

  def puckWillGoToOurNetAfterStrike(self: Hockeyist) = {
    //check if we can strike it safely
    val col = Physics.getCollisionWithWall(world.puck, self.lookVector)
    col != null && col.inNet && col.onOurSide
  }

  object DoDefence extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      //идем к центру ворот
      //если противник будет в зоне опасности раньше - пробуем таранить (проверяем когда пересекли середину поля)
      //иначе разворачиваемся лицом к шайбе в полете
      //если уже у ворот - крутимся за шайбой
    }
  }

  object StrikeToNet extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      //overtime - просто едем к воротам и лупим
    }
  }

  object LookupForPuck extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      if (isPuckOwnedByEnemy && self.canOwnPuck) {
        lastStatus = "owned by enemy and can take/strike\n"
        val strikeIsSafe = {
          if (puckWillGoToOurNetAfterStrike(self)) {
            lastStatus += "do not strike to our net\n"
            false
          }
          else {
            lastStatus += "strike is safe\n"
            true
          }
        }
        move.action = if (strikeIsSafe) Strike else TakePuck
      }
      else if (!isPuckOwnedByEnemy && self.canOwnPuck) {
        lastStatus = "just take it"
        move.action = TakePuck
      }
      else  {
        lastStatus = "go to puck"
        Mover.arriveFor(self, world.puck, move)
      }
    }

  }

  object KickAsses extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      val enemyClosestToPuck = world.hockeyists.filter(_.isMoveableEnemy).sortBy(_.distanceTo(world.puck)).head
      if (self.canPunch(enemyClosestToPuck)) {
        lastStatus = "can strike enemy\n"
        val strikeIsSafe = !self.canOwnPuck || {
          if (puckWillGoToOurNetAfterStrike(self)) {
            lastStatus += "do not strike to our net\n"
            false
          }
          else {
            lastStatus += "strike is safe\n"
            true
          }
        }
        if (strikeIsSafe) {
          lastStatus += "do strike\n"
          move.action = Strike
          return
        }
      }
      //else
      Mover.arriveFor(self, enemyClosestToPuck, move)
    }
  }

  object MakeGoalAlone extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      assert(self.ownPuck)
      lastStatus = ""
      if (Mover.arriveToZone(self, WorldEx.enemyZone.defaultDangerZone, move)) {
        lastStatus += "done, ready to strike"
        move.action = Strike
      }
      else {
        lastStatus += "still arriving to zone\n"
      }
    }
  }
}
