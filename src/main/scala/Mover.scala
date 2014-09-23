import model.ActionType.Swing
import model.HockeyistState.Swinging
import model.{Unit => ModelUnit, _}
import Geometry._
import WorldEx._
import StrictMath._

object Mover {


  val predictStep = 10
  val predictDistance = 300

  def estimatedRandevousPoint(me: Hockeyist, unit: ModelUnit, limit: Double = game.stickLength) = unit.velocityVector match {
    case NullVector() =>
      unit2point(unit)
    case v: Vector =>
      val dist = me.distanceTo(unit)
      if (dist > limit) {
        val coef = me.velocityVector normal_* unit.velocityVector
        val distanceToReac = dist * 0.5 * (2 + coef)
        val estimatedTime = Math.min(200, distanceToReac / unit.velocity)
        //TODO[bug]: когда гонимся за другим хокеистом - стоит учитывать ускорение
        //TODO[bug]: и учитывать куда он смотрит. можно было развернуться раньше и перехватить 14401191e3431322f95f015dcc230776d66ccfeb 4877
        Physics.targetAfter(unit, estimatedTime.toLong)
      }
      else {
        unit2point(unit)
      }
  }

  def arriveFor(me: Hockeyist, unit: ModelUnit, move: Move, limit: Double = game.stickLength): Unit = {
    val estimatedPoint = estimatedRandevousPoint(me, unit, limit)//TODO[fixed]: limit забыл передавать!!!
    me.targetPoints = List(estimatedPoint)
    me.moveVector_target = unit2point(me) -> estimatedPoint
    me.moveVector_enemy = new Vector(0,0)
    me.moveVector = me.moveVector_target + me.moveVector_enemy
    doMove(me, me.moveVector, move)
  }

  val angleEps = 0.01
  val estimationDelta = 20

  def arriveToNetAndStop(me: Hockeyist, move: Move)(whileBrake: =>Unit): Boolean = {
    val zone = WorldEx.myZone.net
    if (zone.includes(me)) {
      me.passFrom = null
      me.passFromArrival = -1
      true
    }
    else {
      //bad reuse
      me.targetPoints = List()

      if (me.passFrom != null && me.passFromArrival < world.tick - estimationDelta) {
        me.passFrom = null
      }

      var speedDown = false

      if (me.passFrom == null) {
        findFirstPointInZone(me, new Zone {
          override def includes(p: Point): Boolean = zone.closerToNetPoint.distanceTo(p) <= me.radius
        },  p => me.targetPoints = p :: me.targetPoints ) match {
          case Some((point, time)) =>
            val targetVelocity = Physics.velocityAfter(me, time)
            if (targetVelocity > 0.6) {
              //need to speed down
              speedDown = true
            }
            else {
              me.passFrom = point
              me.passFromArrival = world.tick + time
            }
          case None =>
        }
      }

      //we know target - do not speedup, just turn to enemy
      if (me.passFrom != null) {
        me.targetPoints = List(me.passFrom)
        whileBrake
        //ok, do nothing
      }
      else {
        val moveVector = me -> zone.middle
        me.moveVector = moveVector
        me.moveVector_enemy = null
        me.moveVector_target = null
        doMove(me, moveVector, move, speedDown)
        if (moveVector.dy ~~ 0 && me.velocityVector.dy ~~ 0) {
          //do not turn - just move backward
          move.turn = 0
        }
      }
      false
    }
  }

  def findFirstPointInZone(me: Hockeyist, zone: Zone, onEachPoint: (Point) => Unit): Option[(Point, Long)] = {
    val velocity = me.velocity
    var firstSkiped = false
    for (dist <- predictStep to (predictDistance, predictStep)) {
      val possibleTarget = Physics.targetAfter(me, (dist / velocity).toLong)
      onEachPoint(possibleTarget)
      //me.targetPoints = possibleTarget :: me.targetPoints
      if (zone.includes(possibleTarget) && !firstSkiped) {
        firstSkiped = true
      }
      else if (zone.includes(possibleTarget)) {
        return Some((possibleTarget, (dist / velocity).toLong))
        /*val whereToPass = if (possibleTarget.isTop) WorldEx.enemyZone.targetBottom else WorldEx.enemyZone.targetTop
        me.passTo = whereToPass
        me.targetPoints = whereToPass :: me.targetPoints
        me.passFrom = possibleTarget
        me.passFromArrival = world.tick + (dist / velocity).toLong*/
      }
    }

    None
  }

  def doTurn(me: Hockeyist, target: Point, move: Move, from: Point = null, ticksLeft: Int = 1): Boolean = {
    val angleAdjustement = Physics.angleAfter(me.angularSpeed, ticksLeft)
    //if (ticksLeft != 0) println(me.angularSpeed, ticksLeft, angleAdjustement)
    val angleTo = me.angleTo(Option(from).getOrElse(me.point), target) - angleAdjustement

    if (Math.abs(angleTo) > angleEps) {
      move.turn = angleTo
      move.speedUp = 0
      true
    }
    else {
      false
    }
  }

  def arriveToZone(me: Hockeyist, zone: PointSpecZone, move: Move, addStatus: (String) => Unit): Boolean = {
    me.inZone = zone.includes(me)
    if (zone.includes(me)) {
      val passFrom = if (me.passFrom != null && me.passFromArrival > world.tick - estimationDelta) me.passFrom else me.point
      me.moveVector = null
      //TODO
      //if (me.passTo == null) {
        me.passTo = if (passFrom.isTop) WorldEx.enemyZone.targetBottom else WorldEx.enemyZone.targetTop
      //}
      if (me.state != Swinging) {
        if (doTurn(me, me.passTo, move, passFrom)) {
          addStatus("doTurn2")
          return false
        }
      }
      val ticksRemaining = me.passFromArrival - world.tick
      val needSwing = WorldEx.enemyZone.needSwingWhenStrikeFrom(passFrom)
      if (needSwing && ticksRemaining > 0 && me.swingTicks < game.maxEffectiveSwingTicks) {
        move.action = Swing
        addStatus("doSwing2")
        return false
      }
      else {
        move.action = ActionType.None
      }
      true
    }
    else {
      //TODO
      if (me.passFrom != null && me.passFromArrival < world.tick - estimationDelta) {
        addStatus("decided to cancel strike from last point")
        me.passFrom = null
      }
      val velocity = me.velocity
      me.targetPoints = List()
      if (me.passFrom == null) {
        findFirstPointInZone(me, zone, p => me.targetPoints = p :: me.targetPoints) match {
          case Some((possibleTarget, ticksToArrival)) =>
            val whereToPass = if (possibleTarget.isTop) WorldEx.enemyZone.targetBottom else WorldEx.enemyZone.targetTop
            val angleTo = me.angleTo(possibleTarget, whereToPass)
            val ticksToTurn = Physics.ticksForTurn(me, angleTo).toLong
            val ticksToSwing = game.maxEffectiveSwingTicks
            if (ticksToArrival <= ticksToSwing + ticksToTurn + 5) {

              me.passTo = whereToPass
              me.targetPoints = whereToPass :: me.targetPoints
              me.passFrom = possibleTarget
              me.passFromArrival = world.tick + ticksToArrival
              addStatus("found point to stop at")
            }
            else {
              addStatus("found point to stop at, but have time to move fwd")
            }
          case None =>
        }
      }

      if (me.passFrom != null) {
        //we may turn
        me.targetPoints = List(me.passFrom, me.passTo)
        val angleTo = me.angleTo(me.passFrom, me.passTo)
        val ticksToTurn = Physics.ticksForTurn(me, angleTo)
        var ticksToArrival = me.passFromArrival - world.tick
        val ticksToSwing = game.maxEffectiveSwingTicks
        //recalculate ticks to arrival - shall not differ
        val ticksToArrival_now = Physics.timeToArrival(me, me.passFrom, ticksToArrival)
        var ticksDelta = 0.0
        if (ticksToArrival_now != ticksToArrival) {
          //ticksDelta = ticksToArrival - ticksToArrival_now
          //ticksToArrival = ticksToArrival_now
          //me.passFromArrival = world.tick + ticksToArrival_now
          addStatus(s"adjusted arrival time from ${ticksToArrival} to ${ticksToArrival_now}")
        }
        //println(s"adjusted arrival time - was ${me.passFromArrival - world.tick} now $ticksToArrival")
        //me.passFromArrival = world.tick + ticksToArrival
        //println(world.tick, ticksToArrival, ticksToSwing, ticksToTurn)
        val canTurn = me.state != Swinging || ticksToArrival_now > game.cancelStrikeActionCooldownTicks  //otherwise there will be no strike
        if (canTurn && doTurn(me, me.passTo, move, me.passFrom, ticksToArrival_now.toInt)) {
          addStatus("do turn1")
          return false
        }
        else {
          addStatus("do swing")
          move.action = Swing
          return false
        }
      }

      addStatus("continue moving")

      val ForwardLen = 35
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

  def doMove(me: Hockeyist, vector: Vector, move: Move, forceSpeeddown: Boolean = false): Unit = {
    val tp = vector(me)
    val product = me.lookVector normal_* vector
    move.speedUp = if (forceSpeeddown) -0.01 else product * 1.1
    move.turn = me.angleTo(tp.x, tp.y)
    //TODO[bug]: 14401191e3431322f95f015dcc230776d66ccfeb 4382 правильнее было бы затормозить. но произведение векторов положительное
  }

}
