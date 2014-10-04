import Geometry.Point
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

  def enemyFreePoint = {
    val enemies = world.hockeyists.filter(_.isMoveableEnemy)
    val sumPoint = enemies.map(_.point).foldLeft(new Point(0,0))((a,b) => new Point(a.x + b.x, a.y + b.y))
    val middlePoint = new Point(sumPoint.x / enemies.length, sumPoint.y / enemies.length)
    val free1 = if (middlePoint.isBottom) middlePoint.toTop else middlePoint.toBottom
    if (free1.inNet || Math.abs(free1.y - middlePoint.y) < 100) {
      if (free1.isLeft) free1.toRight else free1.toLeft
    }
    else {
      free1
    }
  }

  object DoDefence extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      self.targetPoints = List()
      lastStatus = ""
      val net = WorldEx.myZone.net
      val enemyWithPuck = world.hockeyists.find(h => h.isMoveableEnemy && h.ownPuck)
      if (self.ownPuck) {
        var freePoint = enemyFreePoint
        self.targetPoints = List(freePoint)
        //val ally = world.hockeyists.filter(_.isMoveableOur).filter(_.id != self.id).sortBy(h => Math.abs(self.angleTo(h))).head
        lastStatus += "pass puck to area free of enemies"
        if (self.remainingCooldownTicks > 0 || puckWillGoToOurNetAfterStrike(self)) {
          Mover.doTurn(self, freePoint, move)
        }
        else {
          move.action = Pass
          move.passPower = 1.0
          move.passAngle = self.angleTo(freePoint.x, freePoint.y)
        }
        return
      }


      if (!isPuckOwnedByOur && !enemyWithPuck.isDefined && puckWillGoToOurNet(world.puck.velocityVector) && world.puck.velocity > 14) {
        lastStatus += "puck is running to net"
        if (!strikeOrTakeIfCan(self, move)) {
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
            Mover.doTurn(self, world.puck, move, self.passFrom)
            return
          }
        }
        else {
          return
        }
      }
      if (!isPuckOwnedByOur && takeIfCan(self, move)) {
        return
      }
      if ((!isPuckOwnedByEnemy || world.puck.distanceTo(self) > game.worldWidth/2) && strikeEnemyIfCan(self, move)) {
        return
      }

      //TODO: for puck velocity vector == owner's lookvector
      val movingTarget = world.puck//TODO: Physics.targetAfter(enemyWithPuck.getOrElse(world.puck), 40)
      lastStatus += "\n"

      val puckIsSafe = isPuckOwnedByOur || world.puck.onEnemySide
      val nearMiddle = self.distanceTo(WorldEx.myZone.net.middle) < 30


      if (net.includes(self) && (nearMiddle || !puckIsSafe)) {
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
        Mover.doMove2(self, self -> WorldEx.enemyZone.net.middle, move)
      }
    }
  }

  object PreventStrike extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      if (strikeOrTakeIfCan(self, move)) {
        return
      }
      val enemyWithPuck = world.hockeyists.find(h => h.isMoveableEnemy && h.ownPuck).orNull
      if (enemyWithPuck == null) {
        //strange...
        LookupForPuck.move(self, world, game, move)
        return
      }

      val pointAfterSomeTime = Physics.targetAfter(enemyWithPuck, 250, acceleration = true)
      enemyWithPuck.targetPoints = List(pointAfterSomeTime)
      val enemyIsGoingToUs = WorldEx.myZone.half.includes(enemyWithPuck) || (enemyWithPuck.velocity > 1 && Math.signum(enemyWithPuck.speedX) == WorldEx.myZone.dx && WorldEx.myZone.half.includes(pointAfterSomeTime))
      if (enemyIsGoingToUs) {
        lastStatus = "enemy is going to us. intersect it\n"
        val nearestDangerZonePoint = WorldEx.myZone.danger20.nearestTo(enemyWithPuck).toList.sortBy(p => enemyWithPuck.velocityVector normal_* (enemyWithPuck -> p)).reverse.head
        lastStatus += s"we are: ${Physics.positionRelativeOf(self, enemyWithPuck, nearestDangerZonePoint)}\n"
        enemyWithPuck.targetPoints = nearestDangerZonePoint :: self.targetPoints
        if (WorldEx.myZone.danger20.includes(self) || WorldEx.myZone.danger20.includes(enemyWithPuck) || WorldEx.myZone.danger20.includes(world.puck) || nearestDangerZonePoint.distanceTo(enemyWithPuck) < 50) {
          lastStatus += "enemy is here\n"
          KickAsses.move(self, world, game, move)
          lastStatus += KickAsses.lastStatus
          return
        }
        Mover.doMove2(self, self -> nearestDangerZonePoint, move)

      }
      else {
        lastStatus = "enemy is not going to us\n"
        LookupForPuck.move(self, world, game, move)
        lastStatus += LookupForPuck.lastStatus
      }
    }
  }

  object FoolingAround extends Role {
    val pointsToReach = List(WorldEx.enemyZone.net.middle, WorldEx.myZone.net.middle)
    var point: Point = null
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      self.targetVectors = List()
      self.targetPoints = List()
      if (self.ownPuck) {
        if (point == null || point.distanceTo(self) <= 40) {
          point = pointsToReach.filter(p => p != point).toArray.apply((Math.random()*(pointsToReach.length-1)).toInt)
        }
        Mover.doMove3(self, point, (self.point->point)(100), move, predictCollisionsWeight = 100, turnOutPuck = false)
        //move.action = Strike
      }
      else if (self.canOwnPuck) {
        move.action = TakePuck
      }
      else {
        LookupForPuck.move(self, world, game, move)
      }
    }
  }

  object LookupForPuck extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      if (isPuckOwnedByOur) {
        //do nothing just wait
        lastStatus = "owned by us, wait"
        Mover.doMove2(self, self.point -> enemyFreePoint, move)
        return
      }
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
        Mover.arriveFor(self, world.puck/*world.hockeyists.find(h => h.isMoveableEnemy && h.ownPuck).get*/, move)
      }
      else  {
        lastStatus = "go to puck\n"
        world.hockeyists.filter(_.isMoveableEnemy).foreach(h => {
          lastStatus += s"${h.id} is: ${Physics.positionRelativeOf(h, self, (self.velocityVector*10)(self))}\n"
          lastStatus += s"${h.id} will take puck: ${Physics.timeToArrivalForStick(h, world.puck)}\n"
        })
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
      lastStatus = ""
      //else
      Mover.arriveFor(self, enemyClosestToPuck, move)
    }
  }

  object MakeGoalAlone extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
      lastStatus = ""
      world.hockeyists.filter(_.isMoveableEnemy).foreach(h => {
        lastStatus += s"${h.id} is: ${Physics.positionRelativeOf(h, self, (self.velocityVector*10)(self))}\n"
        lastStatus += s"${h.id} will take puck: ${Physics.timeToArrivalForStick(h, world.puck)}\n"
      })


      if (!self.ownPuck)
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

  object MakeGoal2 extends Role {
    override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {

      val inCentralZone = self >> WorldEx.myZone.start && Math.abs(self.y - Geometry.middleY) <  self.radius*4
      val inZone0 = self << WorldEx.myZone.speedupZone1Bottom
      val inZone1 = !inZone0 && self << WorldEx.myZone.speedupZone2Bottom
      val inEnemyCorner = self >> WorldEx.enemyZone.net
      val inEnemyDangerZone = !inZone0 && !inZone1 && !inEnemyCorner

      val looksToOurNet = self.velocityVector.matchesDx(WorldEx.myZone.dx)

      val expectedZone = self.nextZone
      lastStatus = ""


      if (expectedZone == null || expectedZone.includes(self)) {
        val zoneName = if (inCentralZone) "center" else if (inZone0) "zone0" else if (inZone1) "zone1" else if (inEnemyCorner) "enemy corner" else "enemy danger"
        lastStatus = s"in ${zoneName}\n"

        self.nextZone = if (inCentralZone) {
          Mover.doMoveThroughZone(self, WorldEx.myZone.speedupZone1Top + WorldEx.myZone.speedupZone2Top, move, true, appender)
          //Mover.doMoveThroughZone(self, WorldEx.myZone.start, move, false, appender)
        }
        else if (inZone0) {
          Mover.doMoveThroughZone(self, WorldEx.myZone.speedupZone1Top, move, true, appender)
        }
        else if (inZone1/* && !looksToOurNet*/) {
          //Mover.doMoveThroughZone(self, if (world.puck.isTop) WorldEx.myZone.speedupZone2Top else WorldEx.myZone.speedupZone2Bottom, move, false, appender)
          Mover.doMoveThroughZone(self, WorldEx.myZone.speedupZone2Bottom, move, true, appender)
        }
        /*else if (inZone1 && looksToOurNet) {
          Mover.doMoveThroughZone(self, WorldEx.myZone.start, move, false, appender)
        }*/
        else if (inEnemyDangerZone && !looksToOurNet) {
          Mover.arriveToStrike2(self, move, appender) match {
            case Mover.Arrived =>
              move.action = Strike
              null
            case Mover.OnWay =>
              //do nothing
              null
            case Mover.Canceled =>
              //pass to defencer
              move.passAngle = self.angleTo(WorldEx.myZone.net.middle.x, WorldEx.myZone.net.middle.y)
              move.passPower = 1
              move.action = Pass
              null
          }

        }
        else if ((inEnemyDangerZone && looksToOurNet) || inEnemyCorner) {
          Mover.doMoveThroughZone(self, WorldEx.myZone.speedupZone1Top, move, true, appender)
        }
        else {
          //?
          lastStatus += "well, strange...\n"
          Mover.doMoveThroughZone(self, WorldEx.myZone.start, move, false, appender)
        }


      }
      else {
        lastStatus += "continue moving to zone\n"
        Mover.doMoveThroughZone(self, expectedZone, move, false, appender)
      }


    }
  }



}
