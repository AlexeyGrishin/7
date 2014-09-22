import model.ActionType.{CancelStrike, Swing, Strike, TakePuck}
import model.HockeyistState.Swinging
import model.HockeyistType.Goalie
import model.{Unit => ModelUnit, _}

final class MyStrategy extends Strategy {

  import WorldEx._

  var prevSpeedX: Double = 0
  var h: Hockeyist = null

  def doTestSpeedDecreasing( move: Move) {
    if (world.tick < 80) {
      move.speedUp = 0.5
      val estSpeed = prevSpeedX + 0.116*move.speedUp - prevSpeedX/50
      println(s"acc\t${world.tick}\t${-h.speedX}\t${estSpeed}")
      prevSpeedX = -h.speedX
    }
    else if (Math.abs(h.speedX) > 0.1) {
      println(s"brk,${world.tick},${-h.speedX}")
    }
  }

  var oldY = 0.0

  def doTestPuck(move: Move): Unit = {

    if (afterstrike) {
      val goalie = world.hockeyists.find(h => h.playerId == world.myPlayer.get.id && h.hokeyistType == HockeyistType.Goalie)
      if (goalie.isDefined) {
        val sy = Math.abs(goalie.get.y - oldY)
        if (sy > 0)
          println(s"${world.tick}\t$sy")
        oldY = goalie.get.y
      }

      return
    }

    if (world.tick < 40 && !isPuckCouldBeOwned(h)) {
      move.speedUp = 0.5
    }
    else if (h.y < 600 && isPuckCouldBeOwned(h) && !isPuckOwnedBy(h)) {
      move.action = TakePuck
    }
    else if (h.y < 600 && isPuckOwnedBy(h) && Math.abs(h.angle - Math.PI/2) > 0.1 ) {
      move.turn = Math.PI
    }
    else if (isPuckOwnedBy(h) && Math.abs(h.angle - Math.PI/2) <= 0.1 && h.y < 600 ) {
      move.speedUp = 0.5
    }
    else if (isPuckOwnedBy(h) && Math.abs(h.angle + Math.PI/2) > 0.1 && h.y >= 600 ) {
      move.turn = Math.PI
      println(h.angle)
    }
    else if (isPuckOwnedBy(h) && Math.abs(h.angle + Math.PI/2) <= 0.1 && h.y >= 600 ) {
      //else if (isPuckOwnedBy(h) && Math.abs(h.speedX) <= 0.1) {
      move.action = Strike
      afterstrike = true
    }
  }

  var afterstrike = false

  def doTestAngleSpeed(move: Move): Unit = {
    if (world.tick < 80) {
      move.turn = Math.PI
      game.hockeyistTurnAngleFactor
      println(s"acc\t${world.tick}\t${h.angularSpeed}\t${h.angle}")
    }
    else if (Math.abs(h.angularSpeed) > 0.1) {
      println(s"brk,${world.tick},${h.angularSpeed}")
    }

  }

  override def move(self: Hockeyist, world: World, game: Game, move: Move) {
    //if (self.y == 460) return
    WorldEx.init(world, game, self)
    Logger.doLog(world)
    //if (self.hokeyistType == HockeyistType.Versatile) {
    //if (world.tick < 70) {
    val enemy = world.hockeyists.filter(!_.isOur).filter(_.hokeyistType != Goalie).sortBy(_.distanceTo(world.puck)).head


    if (!self.ownPuck) {
      if (self.canOwnPuck) {
        if (enemy.ownPuck && self.canPunch(enemy)) {
          move.action = Strike
        }
        else {
          move.action = TakePuck
        }
      }
      else if (isPuckOwnedByOur) {
        Mover.arriveFor(self, enemy, move)
        if (self.canPunch(enemy)) {
          move.action = Strike
        }
      }
      else {
        Mover.arriveFor(self, world.puck, move)
        if (self.canPunch(enemy)) {
          move.action = Strike
        }

      }
    }
    else if (Mover.arriveToZone(self, WorldEx.enemyZone.danger16, move)) {
      move.action = Strike
    }
    else {
      if (enemy.canOwnPuck) {
        //TODO: pass to ally
        move.action = Strike
      }
      else if (move.speedUp == 0 && move.turn == 0) {
        move.action = Swing
      }
    }

    /*
    1. скорости хокеиста и шайбы складываются. выгоднее лупить по ходу движения. например отрикошетив от борта. иначе - замах. можно рассчитать
          20*power + speed*cos(angle_look - angle_direction)
    2. другая функция убывания - не 1000/х.
    3. при атаке противника могут забить себе же гол. надо принудительно выталкивать из опасной зоны
    4. надо как-то иметь стимул уходить вбок
    5. зажимание в угол
     */

  //}
  if (self.state == Swinging && (move.action != Strike && move.action != Swing && move.action != CancelStrike && move.speedUp != 0)) {
    move.action = CancelStrike
  }
  //else if (world.tick == 70) {
  /*
    move.speedUp = 1
    val p1 = Physics.targetAfter(self, 10, move.speedUp)
    val p2 = Physics.targetAfter(self, 40, move.speedUp)
    val p3 = Physics.targetAfter(self, 90, move.speedUp)
    println(p1, p2, p3)
    self.targetPoints = p1 :: p2 :: p3 :: List()

  }
  else if (world.tick < 70+90) {
    move.speedUp = 1

  }*/
  //doTestPuck(move)
  //}
  self.move = move
  //Logger.logTurn(self, move)
}
}
