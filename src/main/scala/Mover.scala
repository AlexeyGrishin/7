import model.{Unit => ModelUnit, _}
import Geometry._
import WorldEx._

object Mover {
  def doMoveAndStop(me: ModelUnit, target: ModelUnit, move: Move) {
    doMove(me, target, move, needToStopAt = true)
  }

  def doMove(me: ModelUnit, target: ModelUnit, move: Move) {
    doMove(me, target, move, needToStopAt = false)
  }

  def doMove(me: ModelUnit, target: ModelUnit, move: Move, needToStopAt: Boolean) {
    if (!doTurn(me, target, move)) {
      val distance = Geometry.distance(me, target)
      val speed: Double = Math.sqrt(pow2(me.speedX) + pow2(me.speedY))
      if (needToStopAt) {
        if (distance > 100 && speed < 3) move.speedUp = 1
        else if (distance < 100 && speed > 1) move.speedUp = -speed / 3
        else if (distance < 100) {
          move.speedUp = 0
        }
        else move.speedUp = 0.3
      }
      else {
        move.speedUp = 1
      }
    }
  }

  def doTurn(me: ModelUnit, target: ModelUnit, move: Move): Boolean = {
    doTurn(me, target, move, pi(0.1))
  }

  def doTurn(me: ModelUnit, target: ModelUnit, move: Move, angleLimit: Double): Boolean = {
    val angle: Double = me.angleTo(target)
    move.turn = angle
    Math.abs(angle) >= angleLimit
  }

  val predictStep = 10
  val predictDistance = 300

  def arriveFor(me: Hockeyist, unit: ModelUnit, move: Move): Unit = {
    me.moveVector_target = unit2point(me) -> unit2point(unit)
    me.moveVector_enemy = new Vector(0,0)
    me.moveVector = me.moveVector_target + me.moveVector_enemy
    doMove(me, me.moveVector, move)
  }

  val angleEps = 0.01

  def arriveToZone(me: Hockeyist, zone: PointSpecZone, move: Move): Boolean = {
    if (zone.includes(me) && zone.includes(world.puck)) {
      me.moveVector = null
      val angleTo = me.angleTo(unit2point(me), me.passTo)
      if (Math.abs(angleTo) > angleEps) {
        move.turn = angleTo
        move.speedUp = 0
        return false
      }
      me.startedTurning = false
      true
    }
    else {
      val velocity = me.velocity
      me.targetPoints = List()
      var firstSkiped = false
      for (dist <- predictStep to (predictDistance, predictStep)) {
        val possibleTarget = Physics.targetAfter(me, (dist / velocity).toLong)
        me.targetPoints = possibleTarget :: me.targetPoints
        if (zone.includes(possibleTarget) && !firstSkiped) {
          firstSkiped = true
        }
        else if (zone.includes(possibleTarget)) {
          val whereToPass = if (possibleTarget.isTop) WorldEx.enemyZone.targetBottom else WorldEx.enemyZone.targetTop
          me.passTo = whereToPass
          me.targetPoints = whereToPass :: me.targetPoints
          val angleTo = me.angleTo(possibleTarget, whereToPass)
          val ticksToTurn = Physics.ticksForTurn(me, angleTo)
          val ticksToArrival = dist / velocity
          //TODO: anti-freeze
          if (me.startedTurning && ticksToArrival - ticksToTurn > 10) {
            me.startedTurning = false
          }
          if (!me.startedTurning && (ticksToArrival - ticksToTurn > 10)) {  //TODO: swing effective
            //too earl to turn - continue speedup
            move.speedUp = 1.0
          }
          else if (Math.abs(angleTo) > angleEps) {
            me.startedTurning = true
            move.turn = angleTo
            move.speedUp = 0
          }
          return false
        }
      }
      me.startedTurning = false

      val allVectors = zone.nearestTo(me).map(p => (me -> p) /= (if (p.isBottom == me.isBottom) 2000 else 500))
      val halfZone = allVectors.size / 2
      //TODO: forsee enemies movements
      val dangerEnemies = world.hockeyists.filter(!_.isOur).map(h => (me -> unit2point(h)) /= halfZone*400).map(_.reverse)
      me.moveVector_enemy = dangerEnemies.foldLeft(new Vector(0,0))(_ + _)

      //println(zone.nearestTo(me).map(me -> _))
      //println(allVectors)
      val targetVector = allVectors.foldLeft(unit2velocityVector(me))(_ + _)
      //println(targetVector)
      me.moveVector_target = targetVector
      me.moveVector = targetVector + me.moveVector_enemy
      doMove(me, me.moveVector, move)
      false
    }
  }

  def doMove(me: Hockeyist, vector: Vector, move: Move): Unit = {
    move.speedUp = 1.0
    val tp = vector(me)
    move.turn = me.angleTo(tp.x, tp.y)
    //TODO: try other strategies
  }

}
