import model.ActionType.Swing
import model.HockeyistState.Swinging
import model.{Unit => ModelUnit, _}
import Geometry._
import WorldEx._
import StrictMath._

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

  def estimatedRandevousPoint(me: Hockeyist, unit: ModelUnit, limit: Double = 2*game.stickLength) = unit.velocityVector match {
    case NullVector() =>
      unit2point(unit)
    case v: Vector =>
      val dist = me.distanceTo(unit)
      if (dist > limit) {
        val coef = me.velocityVector normal_* unit.velocityVector
        val distanceToReac = dist * 0.5 * (2 + coef)
        val estimatedTime = Math.min(200, distanceToReac / unit.velocity)
        Physics.targetAfter(unit, estimatedTime.toLong)
      }
      else {
        unit2point(unit)
      }
  }

  def arriveFor(me: Hockeyist, unit: ModelUnit, move: Move): Unit = {
    val estimatedPoint = estimatedRandevousPoint(me, unit)
    me.targetPoints = List(estimatedPoint)
    me.moveVector_target = unit2point(me) -> estimatedPoint
    me.moveVector_enemy = new Vector(0,0)
    me.moveVector = me.moveVector_target + me.moveVector_enemy
    doMove(me, me.moveVector, move)
  }

  val angleEps = 0.01
  val estimationDelta = 20

  def arriveToZone(me: Hockeyist, zone: PointSpecZone, move: Move): Boolean = {
    me.inZone = zone.includes(me)
    if (zone.includes(me)) {
      val passFrom = if (me.passFrom != null && me.passFromArrival > world.tick - estimationDelta) me.passFrom else me.point
      me.moveVector = null
      //TODO
      //if (me.passTo == null) {
        me.passTo = if (passFrom.isTop) WorldEx.enemyZone.targetBottom else WorldEx.enemyZone.targetTop
      //}
      val angleTo = me.angleTo(passFrom, me.passTo)
      if (Math.abs(angleTo) > angleEps) {
        move.turn = angleTo
        move.speedUp = 0
        return false
      }
      val ticksRemaining = me.passFromArrival - world.tick
      val needSwing = WorldEx.enemyZone.needSwingWhenStrikeFrom(passFrom)
      if (needSwing && ticksRemaining > 0 && me.swingTicks < game.maxEffectiveSwingTicks) {
        move.action = Swing
      }
      else {
        move.action = ActionType.None
      }
      me.startedTurning = false
      true
    }
    else {
      //TODO
      if (me.passFrom != null && me.passFromArrival < world.tick - estimationDelta) {
        println("decided to cancel strike from last point")
        me.passFrom = null
        me.startedTurning = false
      }
      val velocity = me.velocity
      me.targetPoints = List()
      if (me.passFrom == null) {
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
            me.passFrom = possibleTarget
            me.passFromArrival = world.tick + (dist / velocity).toLong
          }
        }

      }

      if (me.passFrom != null) {
        me.targetPoints = List(me.passFrom, me.passTo)
        val angleTo = me.angleTo(me.passFrom, me.passTo)
        val ticksToTurn = Physics.ticksForTurn(me, angleTo)
        var ticksToArrival = me.passFromArrival - world.tick
        val ticksToSwing = game.maxEffectiveSwingTicks
        //recalculate ticks to arrival - they shall be smaller as we continue speedup
        val ticksToArrival_now = Physics.timeToArrival(me, me.passFrom, ticksToArrival)
        var ticksDelta = 0.0
        if (ticksToArrival_now < ticksToArrival) {
          ticksDelta = ticksToArrival - ticksToArrival_now
          ticksToArrival = ticksToArrival_now
          me.passFromArrival = world.tick + ticksToArrival_now
        }
        //println(s"adjusted arrival time - was ${me.passFromArrival - world.tick} now $ticksToArrival")
        //me.passFromArrival = world.tick + ticksToArrival
        println(world.tick, ticksToArrival, ticksToSwing, ticksToTurn)
        if (!me.startedTurning && (ticksToArrival - ticksToTurn - ticksToSwing > 2*ticksDelta)) {  //TODO: swing effective
          //too earl to turn - continue speedup
          move.speedUp = 1.0
          return false
        }
        else if (Math.abs(angleTo) > angleEps) {
          me.startedTurning = true
          move.turn = angleTo
          move.speedUp = 0
          return false
        }
        else {
          //?
          move.action = Swing
          return false
        }
      }

      me.startedTurning = false

      val ForwardLen = 30
      val FarEnemy = 5
      val NearEnemy = 20

      val allTargetPoints = zone.nearestTo(me).map(p => (me -> p)(ForwardLen))
      val enemies = world.hockeyists.filter(_.isMoveableEnemy).map(h => {
        val p = estimatedRandevousPoint(me, h, 0)
        me.targetPoints = p :: me.targetPoints
        val distToMe = p.distanceTo(me)
        val v = if (p != h.point) {
          //(h -> p).ort.orient(p,me)
          p -> me.point
        }
        else {
          h.lookVector
        }
        v(Math.max((distToMe / (game.stickLength*3)) * FarEnemy, FarEnemy))
      })



      val enemyVector = enemies.foldLeft(new Vector(0,0))(_ + _)
      val (targetVector, fullVector, product) = allTargetPoints.map(vector => {
        val vecWithEnemy = vector + enemyVector
        val product = vecWithEnemy * me.velocityVector
        (vector, vecWithEnemy, product)
      }).toList.sortBy(_._3).reverse.head
      me.moveVector_enemy = enemyVector
      me.moveVector_target = targetVector
      me.moveVector = fullVector

      doMove(me, me.moveVector, move)
      false
    }
  }

  def doMove(me: Hockeyist, vector: Vector, move: Move): Unit = {
    val tp = vector(me)
    val product = me.lookVector normal_* vector
    move.speedUp = product + 0.1
    move.turn = me.angleTo(tp.x, tp.y)
    //TODO: try other strategies
  }

}
