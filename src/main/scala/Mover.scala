import model.ActionType.{Strike, Swing}
import model.HockeyistState.Swinging
import model.{Unit => ModelUnit, _}
import Geometry._
import WorldEx._
import StrictMath._

object Mover {


  val predictStep = 20
  val predictDistance = 400


  def estimatedRandevousPoint(me: Hockeyist, unit: ModelUnit, limit: Double = game.stickLength) = unit.velocityVector match {
    case NullVector() =>
      unit2point(unit)
    case v: Vector =>
      val dist = me.distanceTo(unit)
      val coef = me.velocityVector normal_* unit.realActor.velocityVector
      val distanceToReac = dist * 0.5 * (2 + coef)
      val estimatedTime = distanceToReac / unit.realActor.velocity//Math.min(200, distanceToReac / unit.velocity)
      val estPoint = Physics.targetAfter(unit, estimatedTime.toLong, acceleration = true)
      me.targetPoints = List()

      val MaxAttempts = 4
      def adjustEstimate(estPoint: Point, estimatedTime: Double, attempts: Int = MaxAttempts): Point = {

        if (attempts == 0) {
          return estPoint
        }
        //TODO[2]: берем угол
        val ang = Math.abs(me.angleTo(estPoint.x, estPoint.y))
        if (ang < Math.toRadians(if (me.justAdjusted) 40 else 10)) {  //sometimes after we turn a bit the angle between us and target becomes > 5 deg
          me.justAdjusted = false
          //we are going straight forward, calculate arrival
          val estOurPoint = Physics.targetAfter(me, estimatedTime.toLong, acceleration = true, analyzeColision = false)
          me.targetPoints = estOurPoint :: me.targetPoints
          val coef = me.distanceTo(estOurPoint) / me.distanceTo(estPoint)
          val lenDiff = 1 - coef
          if (Math.abs(lenDiff) > 0.3) {
            val newTimeEstimate = (estimatedTime * (if (coef > 1) 0.8 else 1.2) ).toLong// Physics.timeToArrival(me, estPoint, (estimatedTime*coef).toLong, me.realSpeedup())
            //println(s"${world.tick}:  reestimated from $estimatedTime to $newTimeEstimate")
            //reestimate
            val newEstPoint = Physics.targetAfter(unit, newTimeEstimate, acceleration = true)
            me.targetPoints = newEstPoint :: me.targetPoints
            val res = adjustEstimate(newEstPoint, newTimeEstimate, attempts - 1)
            me.justAdjusted = true
            res
          }
          else {
            estPoint
          }

        }
        else {
          estPoint
        }
      }



      if (dist > limit && estPoint.distanceTo(me) > limit) {
        //TODO[bug]: когда гонимся за другим хокеистом - стоит учитывать ускорение
        //TODO[bug]: и учитывать куда он смотрит. можно было развернуться раньше и перехватить 14401191e3431322f95f015dcc230776d66ccfeb 4877
        adjustEstimate(estPoint, estimatedTime)
      }
      else {
        unit2point(unit)
      }
  }

  def arriveFor(me: Hockeyist, unit: ModelUnit, move: Move, limit: Double = game.stickLength): Unit = {

    val estimatedPoint = estimatedRandevousPoint(me, unit, limit)
    me.targetPoints = estimatedPoint :: me.targetPoints
    me.moveVector_target = unit2point(me) -> estimatedPoint
    me.moveVector_enemy = new Vector(0,0)
    me.moveVector = me.moveVector_target + me.moveVector_enemy
    doMove2(me, me.moveVector, move)
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
          override def toTop: Zone = null
          override def toBottom: Zone = null
          override val borderPoints: Traversable[Point] = null
          override val targetPoints: Traversable[Point] = null
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
        doMove3(me, zone.middle, moveVector, move)
        //if (moveVector.dy ~~ 0 && me.velocityVector.dy ~~ 0) {
          //do not turn - just move backward
          //move.turn = 0
        //}
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
      if (zone.includes(possibleTarget) && !firstSkiped) {
        firstSkiped = true
      }
      else if (zone.includes(possibleTarget)) {
        return Some((possibleTarget, (dist / velocity).toLong))
      }
    }

    None
  }

  def findFirstPointInZone2(me: Hockeyist, onEachPoint: ((Point, Long)) => Boolean): Option[(Point, Long)] = {
    val velocity = me.velocity
    var firstSkiped = false
    for (dist <- predictStep to (predictDistance, predictStep)) {
      val possibleTarget = Physics.targetAfter(me, (dist / velocity).toLong)
      if (onEachPoint(possibleTarget, (dist / velocity).toLong)) {
        return Some((possibleTarget, (dist / velocity).toLong))
      }
    }

    None
  }

  def doTurn(me: Hockeyist, target: Point, move: Move, from: Point = null, ticksLeft: Int = 1): Boolean = {
    val angleAdjustement = Physics.angleAfter(me.angularSpeed, ticksLeft)
    //println(s"${world.tick}: ${me.angleTo(Option(from).getOrElse(me.point), target)} ${angleAdjustement}")
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

  case class StrikePoint(point: Point, swingFrom: Double, strikeFrom: Double, target: Point) {
    var lastDistance: Double = 0
  }

  val Arrived = "arrived"
  val OnWay = "on way"
  val Canceled = "canceled"

  /*
      if strike-point is unknown
        try to find it. for each possible point
          measure time to arrive
          measure time for enemies to arrive
          measure cooldown
          decision = point + swing/
   */
  def arriveToStrike2(me: Hockeyist, move: Move, addStatus: (String) => Unit): String = {
    val zoneToStrike20 = WorldEx.enemyZone.danger20.to(me)
    val zoneToStrike16 = WorldEx.enemyZone.danger16.to(me)
    val strikeTo = if (me.isBottom) WorldEx.enemyZone.targetTop else WorldEx.enemyZone.targetBottom
    val enemiesThatCouldBother = world.hockeyists.filter(_.isMoveableEnemy).filter(_.remainingCooldownTicks == 0)

    if (me.strikePoint == null) {
      me.targetPoints = List(strikeTo)
      //look for point
      findFirstPointInZone2(me, p => {
        val (possibleTarget, ticksToArrival) = p
        me.targetPoints = possibleTarget :: me.targetPoints

        val realTicksToArrival = Physics.timeToArrivalDirect(me, possibleTarget)
        val angleTo = me.angleTo(possibleTarget, strikeTo)
        val ticksToTurn = Physics.ticksForTurn(me, angleTo)
        val ticksToCooldown = me.remainingCooldownTicks
        val ticksToSwing = game.maxEffectiveSwingTicks

        val enemiesArrival = (enemiesThatCouldBother.map(e => Physics.timeToArrivalForStick(e, possibleTarget)) ++ List(999.0)).min

        addStatus(f"ta = ${realTicksToArrival.toInt}%d, tt = ${ticksToTurn.toInt}%d, tc = ${ticksToCooldown.toInt}%d, te = ${enemiesArrival.toInt} ")

        val targetVelocity = Physics.velocityAfter(me, realTicksToArrival.toLong)
        //TODO: stamina, etc.
        val estimatedPuckSpeedWithoutSwing_old = 20*0.75 + targetVelocity * (me.lookVector normal_*(me.velocityVector))
        val estimatedPuckSpeedWithoutSwing = me.puckSpeedAfterStrike(0, targetVelocity)
        addStatus(s"point($possibleTarget)")
        if (realTicksToArrival < Math.max(ticksToTurn, ticksToCooldown)) {
          //i will not turn for the arrival time
          addStatus(s" - no time to turn")
          false
        }
        else {
          val timeCanSpendOnSwing = Math.min(game.maxEffectiveSwingTicks, if (realTicksToArrival > enemiesArrival) {
            addStatus(s" - enemies are close. no swing")
            0
          }
          else {
            Math.max(0, realTicksToArrival - Math.max(ticksToTurn, ticksToCooldown))
          })
          val realTimeCanSpendOnSwing = if (timeCanSpendOnSwing < game.swingActionCooldownTicks) 0 else timeCanSpendOnSwing
          val estimatedPuckSpeedWithSwing_old = estimatedPuckSpeedWithoutSwing + 20 * 0.25*(realTimeCanSpendOnSwing / game.maxEffectiveSwingTicks)
          val estimatedPuckSpeedWithSwing = me.puckSpeedAfterStrike(realTimeCanSpendOnSwing, targetVelocity)
          addStatus(s" - s=$estimatedPuckSpeedWithoutSwing, $estimatedPuckSpeedWithSwing")
          println(s" - s=$estimatedPuckSpeedWithoutSwing(old=$estimatedPuckSpeedWithoutSwing_old), $estimatedPuckSpeedWithSwing(old=$estimatedPuckSpeedWithSwing_old)")
          val remaining = realTicksToArrival - Math.max(ticksToTurn, ticksToCooldown) - realTimeCanSpendOnSwing
          if (remaining > 5) {
            addStatus(s" - big remaining - better wait")
            false
          }
          else if (estimatedPuckSpeedWithSwing >= 20 && zoneToStrike20.includes(possibleTarget)) {
            addStatus(s" - zone 20")
            me.strikePoint = new StrikePoint(possibleTarget, world.tick + realTicksToArrival - timeCanSpendOnSwing, world.tick + realTicksToArrival, strikeTo)
            me.strikePoint.lastDistance = me.distanceTo(possibleTarget)
            true
          }
          else if (estimatedPuckSpeedWithSwing >= 16 && zoneToStrike16.includes(possibleTarget)) {
            addStatus(s" - zone 16")
            me.strikePoint = new StrikePoint(possibleTarget, world.tick + realTicksToArrival - timeCanSpendOnSwing, world.tick + realTicksToArrival, strikeTo)
            me.strikePoint.lastDistance = me.distanceTo(possibleTarget)
            true
          }
          else {
            addStatus(s" - too far")
            false
          }
        }
      })
      if (me.strikePoint == null) {
        doMove3_avoidCollisions(me, enemyZone.net.middle, move)
        //doMoveThroughZone(me, zoneToStrike20, move, false, addStatus)
      }
      OnWay
    }
    else {
      me.targetPoints = List(me.strikePoint.point, me.strikePoint.target)
      val newDistance = me.distanceTo(me.strikePoint.point)
      if (newDistance - me.strikePoint.lastDistance > 5) {
        addStatus(s"being far from strike point. cancel")
        me.strikePoint = null
        return Canceled
      }
      me.strikePoint.lastDistance = newDistance
      me.targetVectors = List(("target", me.strikePoint.point->me.strikePoint.target))
      if (world.tick < me.strikePoint.strikeFrom/*world.tick < me.strikePoint.swingFrom &&*/ ) {
        if (me.state == HockeyistState.Swinging || !doTurn(me, me.strikePoint.target, move, me.strikePoint.point, (me.strikePoint.strikeFrom - world.tick).toInt)) {
          move.action = Swing
          addStatus(s"no need to turn. swing. strike after ${me.strikePoint.strikeFrom - world.tick}")
        }
        else {
          addStatus(s"turn. swing after ${me.strikePoint.swingFrom - world.tick}. strike after ${me.strikePoint.strikeFrom - world.tick}")
        }
        OnWay
      }
      //else if (world.tick < me.strikePoint.strikeFrom) {
      //  move.action = Swing
      //  addStatus(s"swing. strike after ${me.strikePoint.strikeFrom - world.tick}")
      //  OnWay
      //}
      else {
        move.action = Strike
        addStatus("Strike")
        Arrived
      }
    }

  }

  @Deprecated
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
        val ticksToArrival_now = Physics.timeToArrivalDirect(me, me.passFrom)
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
      val NearEnemy = 15
      val Wall = 6

      val allTargetPoints = zone.nearestTo(me).map(p => (me -> p)(ForwardLen))
      val enemies = world.hockeyists.filter(_.isMoveableEnemy).map(h => {
        val p = estimatedRandevousPoint(me, h, game.stickLength)
        me.targetPoints = p :: me.targetPoints
        val distToMe = p.distanceTo(me)
        val v = if (p != h.point) {
          //(h -> p).ort.orient(p,me)
          p -> me.point
        }
        else {
          //h.lookVector
          p -> me.point //TODO: not sure we need use "lookvector" when enemy is standby
        }
        //v(Math.max(((game.stickLength*2) / distToMe) * FarEnemy, NearEnemy))
        v(Math.max(FarEnemy, FarEnemy + ((NearEnemy - FarEnemy)*(1 - distToMe/(game.stickLength)))))
      })


      val enemyVector = enemies.foldLeft(new Vector(0,0))(_ + _)
      val wallVector = normalFromNearestWalls(me, 5*me.radius) * Wall


      val (targetVector, fullVector, product) = allTargetPoints.map(vector => {
        val vecWithEnemy = vector + enemyVector + wallVector
        val product = vecWithEnemy * me.velocityVector
        (vector, vecWithEnemy, product)
      }).toList.sortBy(_._3).reverse.head
      me.moveVector_enemy = enemyVector
      me.moveVector_target = targetVector
      me.moveVector = fullVector
      me.targetVectors = ("from wall", wallVector) :: Nil

      doMove2(me, me.moveVector, move)
      false
    }
  }

  def normalFromNearestWalls(p: ModelUnit, limit: Double) = {
    val radius = p.radius - 5 //pad - sometimes some part of hockeyist goes through wall
    val vec = List(
      new Vector(p.x - radius - game.rinkLeft, 0),
      new Vector(0, p.y - radius - game.rinkTop),
      new Vector(p.x + radius - game.rinkRight, 0),
      new Vector(0, p.y + radius - game.rinkBottom)
    ).filter(_.length < limit).foldLeft(new Vector(0, 0))(_ + _)
    vec(1 - vec.length / limit)*2

  }

  def doMove3_avoidCollisions(me: Hockeyist, target: Point, move: Move, turnOutPuck: Boolean = false): Unit = {
    doMove3(me, target, (me.point->target)(100), move, predictCollisionsWeight = 40, turnOutPuck = turnOutPuck)

  }
  def doMove3(me: Hockeyist, target: Point,vector: Vector,  move: Move, predictCollisionsWeight: Double = 0.0, turnOutPuck: Boolean = false): Unit = {
    me.moveVector_target = vector
    me.moveVector = vector
    val currentVelocityVector = me.velocityVector
    //1. predict colisions
    if (predictCollisionsWeight > 0) {
      val avoidVectors = world.hockeyists.filter(h => h.id != me.id).map(h => (h, Physics.getAvoidanceVector(me, currentVelocityVector, me.distanceTo(target), h)))
      me.targetVectors = me.targetVectors ++ avoidVectors.map(v => ("avoid", v._2*50))
      val avoidVector = Vector.sum(avoidVectors.map(_._2))*predictCollisionsWeight
      me.targetVectors =  ("main_avoid", avoidVector) :: me.targetVectors
      me.moveVector = me.moveVector + avoidVector
    }

    val enemiesCloseToPuck = world.hockeyists.filter(h => h.isMoveableEnemy && h.distanceTo(world.puck) < 2*game.stickLength)
    val turnTarget = me.moveVector.normal(me)
    val angleToTurn = me.angleTo(turnTarget.x, turnTarget.y)
    move.turn = angleToTurn
    //2. turn out puck TODO: does not work
    if (me.ownPuck && turnOutPuck && enemiesCloseToPuck.nonEmpty) {
      val angleDirectCouterEnemy = Math.signum(enemiesCloseToPuck.map(e => -me.angleTo(e)).sum)
      val nearest = enemiesCloseToPuck.map(_.distanceTo(world.puck)).min
      if (angleToTurn > angleEps && Math.signum(angleToTurn) == angleDirectCouterEnemy) {
      }
      else {
        move.turn += (Math.PI * (1 - Math.max(0, nearest - game.stickLength) / game.stickLength)) * angleDirectCouterEnemy
      }
      println(s"${world.tick}: angle is ${angleToTurn}, enemiesDirection = ${angleDirectCouterEnemy}, result = ${move.turn}")
    }
    else {
      move.turn = angleToTurn
    }
    val product = me.lookVector normal_* me.moveVector
    move.speedUp = (product*2-1)*1.1


  }


  def doMove2(me: Hockeyist, vector: Vector, move: Move, forceSpeeddown: Boolean = false): Unit = {
    me.moveVector = vector
    val tp = vector(me)
    val product = me.lookVector normal_* vector
    move.speedUp = (product*2-1)*1.1
    move.turn = me.angleTo(tp.x, tp.y)
    /*if (me.ownPuck && world.hockeyists.filter(_.isMoveableEnemy).exists(_.distanceTo(me) < game.stickLength)) {
      val en = world.hockeyists.filter(_.isMoveableEnemy).find(_.distanceTo(me) < game.stickLength).get
      val angleToEnemy = me.angleTo(en)

      move.turn = -move.turn
    }*/
  }


  def doMoveThroughZone(me: Hockeyist, zone: Zone, move: Move, allowChangeDirection: Boolean, report: (String) => Unit) = {
    val zoneTop = zone.toTop
    val zoneBottom = zone.toBottom
//    val enemies = world.hockeyists.filter(_.isMoveableEnemy)
    val zoneToAnalyze = if (allowChangeDirection) zoneTop.targetPoints ++ zoneBottom.targetPoints else zone.targetPoints

    val ForwardLen = 50
    val FarEnemy = 5
    val NearEnemy = 10
    val Wall = 6

    me.targetPoints = List()

    me.targetVectors = List()
    val vectorFromAllies = world.hockeyists.filter(_.isMoveableOur).filter(_.id != me.id).filter(_.distanceTo(me) < 120).map(_.point -> me.point).map(_.apply(Wall)).foldLeft(new Vector(0,0))(_+_)
    val (targetVector, fullVector, product, point, enemyVector) = zoneToAnalyze.map(point => {
      val enemyFactor = world.hockeyists.filter(_.isMoveableEnemy).map(h => {
        //me.targetVectors = me.targetVectors ++ List(("", h.point -> me.point), ("", h.point -> point))
        (h.point -> me.point)*(h.point -> point)
      }).sum
      val enemyVector = world.hockeyists.filter(_.isMoveableEnemy).filter(h => Physics.timeToArrivalForStick(h, me) < 50).map(h => h.point -> me.point).map(_.apply(NearEnemy)).foldLeft(new Vector(0,0))(_+_)
      val vector = (me -> point)(ForwardLen)
      val distance = me.distanceTo(point.x, point.y)
      val vecWithEnemy = vector + enemyVector + vectorFromAllies
      val product = (vecWithEnemy normal_* me.lookVector) //((enemyVector normal_* me.velocityVector)
      (vector, vecWithEnemy, product, point, enemyVector)
    }).toList.sortBy(_._3).reverse.head
    me.moveVector_enemy = enemyVector
    me.moveVector_target = targetVector
    me.moveVector = fullVector
    me.targetPoints = List(point)
    me.targetVectors = List(("", vectorFromAllies))


    doMove3(me, point, fullVector, move, predictCollisionsWeight = 20)
    if (allowChangeDirection)
      if (zoneTop.includes(point)) zoneTop else zoneBottom
    else
      zone
  }

}
