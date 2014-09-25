import model.ActionType.{Pass, TakePuck, Strike}
import model.{Unit => ModelUnit, Hockeyist, World, Game, Move, Puck}

import WorldEx._

object Roles {

  def puckWillGoToOurNetAfterStrike(self: Hockeyist) = {
    puckWillGoToOurNet(self.lookVector)
  }

  def puckWillGoToOurNet(vec: Geometry.Vector) = {
    val col = Physics.getCollisionWithWall(world.puck, vec)
    col != null && col.inNet && col.onOurSide
  }

  def puckWillGoToEnemyNet(vec: Geometry.Vector) = {
    val col = Physics.getCollisionWithWall(world.puck, vec)
    col != null && col.inNet && col.onEnemySide
  }

  object DoNothing extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      //nothing
    }
  }

  object DoDefence extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      self.targetPoints = List()
      lastStatus = ""
      val net = WorldEx.myZone.net
      val enemyWithPuck = world.hockeyists.find(h => h.isMoveableEnemy && h.ownPuck)
      if (self.ownPuck) {
        val ally = world.hockeyists.filter(_.isMoveableOur).filter(_.id != self.id).sortBy(h => Math.abs(self.angleTo(h))).head
        lastStatus += "pass puck to ally"
        if (self.remainingCooldownTicks > 0 || puckWillGoToOurNetAfterStrike(self)) {
          Mover.doTurn(self, ally, move)
        }
        else {
          move.action = Pass
          move.passPower = 1.0
          move.passAngle = self.angleTo(ally)
        }
        return
      }

      def ownPuckIfCan() = if (self.canOwnPuck) {
        if (puckWillGoToOurNetAfterStrike(self)) {
          lastStatus += "\ntake!"
          move.action = TakePuck
        }
        else {
          lastStatus += "\nstrike!"
          move.action = Strike
        }
        true
      } else false


      if (ownPuckIfCan()) {
        return
      }

      if (!isPuckOwnedByOur && !enemyWithPuck.isDefined && puckWillGoToOurNet(world.puck.velocityVector)) {
        lastStatus += "puck is running to net"
        if (!ownPuckIfCan()) {
          val rp = Mover.estimatedRandevousPoint(self, world.puck)
          if (rp.onOurSide) {
            lastStatus += "\ngo to it"
            self.passFrom = null //TODO: иначе arriveForNet может продолжать думать будто мы все еще идем к точке
            //TODO: если идет сквозь меня - просто развернуться
            Mover.arriveFor(self, world.puck, move, limit = 3*game.stickLength)
            return
          }
          else {
            lastStatus += "\nis on enemy side, wait"
          }
        }
      }

      //TODO[fixed]: never leave net
      /*
      if (enemyWithPuck.isDefined && !net.includes(self)) {
        val enemy = enemyWithPuck.get
        if (WorldEx.myZone.danger20.includes(enemy)) {
          lastStatus += "enemy in danger zone, we - not"
        }
        else {
          val nearestZonePoint = WorldEx.myZone.danger20.borderPoints.sortBy(_.distanceTo(enemy)).head
          //TODO[fixed]: когда враг уже на нашей половине, то уже поздно. я бы брал 3/4
          val goingThere = enemy.velocity > 0 /*&& enemy.point.onOurSide*/ && StrictMath.signum(enemy.velocityVector.dx) == WorldEx.myZone.dx
          if (goingThere) {
            lastStatus += "enemy is going here"
            //TODO[bug]: учитывать во всех случаях направление. если я смотрю не в сторону ворот, то время будет гораздо больше. в лоб решение не катит - идет назад, поворот не нужен
            val timeToZone = Physics.timeToArrival(enemy, nearestZonePoint, (nearestZonePoint.distanceTo(enemy) / enemy.velocity).toInt)
            //val timeToTurn = Physics.ticksForTurn(self, self.angleTo(self.point, net.middle))
            val timeToNet = Physics.timeToArrival(self, net.middle, (net.middle.distanceTo(self) / self.velocity).toInt)

            if (timeToZone > timeToNet/* || self.onOurSide*/) {//TODO: onOurSide?
              lastStatus += "\n continue going to net"
            }
            else {
              lastStatus += "\n no time for net. Time to kick ass"
              if (!ownPuckIfCan) {
                self.role = KickAsses
                self.role.move(self, world, game, move)
                self.role.lastStatus = lastStatus + "\n\n" + self.role.lastStatus
              }
              return
            }
          }
          else {
            lastStatus += "enemy is not going here"
          }
        }
      }*/
      //TODO: for puck velocity vector == owner's lookvector
      val movingTarget = world.puck//TODO: Physics.targetAfter(enemyWithPuck.getOrElse(world.puck), 40)
      lastStatus += "\n"

      if (net.includes(self)) {
        Mover.doTurn(self, movingTarget, move, self.passFrom)
        lastStatus += "look for puck"
      }
      else {
        lastStatus += "go to net"
        Mover.arriveToNetAndStop(self, move) {
          Mover.doTurn(self, movingTarget, move, self.passFrom)
        }
      }
      self.targetPoints = movingTarget :: self.targetPoints
    }
  }

  object StrikeToNet extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      //overtime - просто едем к воротам и лупим
      if (puckWillGoToEnemyNet(self.lookVector)) {
        lastStatus = "strike to enemy net"
        move.action = Strike
      }
      else {
        lastStatus = "turn to enemy net"
        Mover.doMove(self, self -> WorldEx.enemyZone.net.middle, move)
      }
    }
  }

  object FoolingAround extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      if (self.canOwnPuck) {
        move.action = Strike
      }
      else {
        LookupForPuck.move(self, world, game, move)
      }
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
      else if (isPuckOwnedByEnemy) {
        lastStatus = "go to puck in enemy hands"
        Mover.arriveFor(self, world.hockeyists.find(h => h.isMoveableEnemy && h.ownPuck).get, move)
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
      lastStatus = ""
      if (!self.ownPuck)  //TODO[fixed] убрать, предусмотреть
      {
        lastStatus += "puck is not mine O_o\n"
        LookupForPuck.move(self, world, game, move)
        lastStatus += LookupForPuck.lastStatus
        return
      }
      if (Mover.arriveToZone(self, WorldEx.enemyZone.defaultDangerZone, move, s => lastStatus += s + "\n")) {
        lastStatus += "done, ready to strike"
        move.action = Strike
      }
      else {
        lastStatus += "still arriving to zone\n"
      }
    }
  }
}
