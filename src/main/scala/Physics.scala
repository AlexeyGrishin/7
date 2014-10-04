import WorldEx._
import Geometry.{Line, Point, Vector}
import model.Hockeyist
import model.{Unit => ModelUnit}
import StrictMath._

object Physics {
  //hockeist:
  //    agility = 0.75*agility + 0.25*(stamina/2000)*agility
  //  v(t) = v(t-1) + hockeyistSpeedUpFactor*speedup*(agility/100) - v(t-1)/50

  //  v(t) = v(t-1)*0.98 + SP
  //  v(t) = (v(t-2)*0.98 + SP)*0.98 + SP = v(t-2)*0.98^2 + SP*(1 + 0.98)
  //  v(t) = v0 * K^t + SP*(sum[0,t] K^t-1) = v0* K^t + SP* (K^t - 1) / K*ln(K)
  //                  v0* K^t + SP* K^t / K*ln(K) - SP / K*ln(K)

  //  d(t) = d0 + E[0,t](v0 + K^t + SP*K^(t-1) / ln(K))
  //        = d0 + v0* K^t/ln(K)+ SP*K^t / K / ln(K) / lm(K) - SP*t/K/ln(K)

  //  angle(t) = angle(t-1) + hockeyistTurnAngleFactor*(agility/100)
  //                          ^= TK
  //    t = angleTarget / TK
  //  angle(n) = angle1 + n*(0.75*HTAF*agility/100 + 0.25*stamina1/2000*agility/100) - sum(1,n)*	0.25/2000*agility/100

  //puck\
  //v(t) = v(t-1) - v(t-1)/1000

  def angleDiff(a1: Double, a2: Double) = {
    var relativeAngleTo = a2 - a1
    while (relativeAngleTo > PI) {
      relativeAngleTo -= 2.0D * PI
    }

    while (relativeAngleTo < -PI) {
      relativeAngleTo += 2.0D * PI
    }
    relativeAngleTo
  }

  def ticksForTurn(hock: Hockeyist, turnFor: Double) = {
    //val delta = angleDiff(hock.angle, turnFor)
    Math.ceil(Math.abs(turnFor) / hock.realTurnspeed)
  }

  def integ(to: Double, ifn: (Double) => Double) = ifn(to) - ifn(0)
  def integ(from: Double, to: Double, ifn: (Double) => Double) = ifn(to) - ifn(from)

  val Following = "following"
  val OnWay = "on way"
  val Other = "far from"
  val Intersect = "intersect"

  /// who -> mover ---> target      Following
  /// mover --> who ---> target     OnWay
  /// mover ---> target <--- who    Intersect


  def positionRelativeOf(who: Hockeyist, mover: Hockeyist, target: Point) = {
    val mover2target = new Line(mover.point, target)
    val dot = mover2target.normal * (who.point -> mover.point)
    val distance = mover2target.distanceTo(who)

    if (dot > 0.7 && distance <= who.radius*1.5) {
      //same line, following
      Following

    }
    else if (dot < -0.7 && distance <= who.radius*1.5) {
      //same line, before/between
      if ((who.point->target) * mover2target.normal > 0.7)
        OnWay
      else
        Intersect
    }
    else {
      //other
      Other
    }
  }

  def isEnemyOnWay(me: Hockeyist, target: Point, enemy: Hockeyist) = {
    positionRelativeOf(enemy, me, target) == OnWay
  }

  //rturn vector len = 1 - directly on way < radius*prec, ~0 - > radius*prec
  //                 = 1 - distance <= radius*prec * 2, ~0 - distance is ~1000
  def getAvoidanceVector(me: Hockeyist, vector: Vector, maxDistance: Double, obj: ModelUnit, prec: Double = 1.5) = {
    val line = vector.toLine(me.point)
    val normalFromObj = line.ortVector(obj)
    val colDistance = me.radius + obj.radius
    if (normalFromObj.length <= colDistance*prec && me.distanceTo(obj) < maxDistance && line.directionTo(obj)) {
      val avoidanceVector = if (normalFromObj.length == 0) {
        vector.ort
      } else normalFromObj
      avoidanceVector.normal * ( 1.01 - Math.max(0, normalFromObj.length - colDistance) / (colDistance*prec - colDistance)) * (1.01 - Math.max(0, me.distanceTo(obj) - 2*colDistance) / 1000)
    }
    else {
      new Vector(0,0)
    }
  }


  def timeToArrivalForStick(hock: Hockeyist, target: Point) = {
    hock.timeToGetPuck = if (hock.distanceTo(target) <= game.stickLength+5) {
      hock.timeToGetPuckStat = "just turn"
      timeToTurn(hock, target, game.stickSector/2)
    }
    else {
      if (Math.abs(hock.angleTo(target.x, target.y)) < toRadians(10)) {
        hock.timeToGetPuckStat = "go directly"
        timeToArrivalDirect(hock, ((hock.point -> target) - game.stickLength)(hock.point), hock.realSpeedup())
      }
      else {
        hock.timeToGetPuckStat = "go and turn"
        val tturn = timeToTurn(hock, target, game.stickSector/4)
        //here I do not substract game.stickLength as actually enemy will spend more time - as during turn he will loose speed
        val ttarrival = timeToArrivalDirect(hock, target, hock.realSpeedup())
        tturn + ttarrival
      }
    }
    hock.timeToGetPuck
  }

  def timeToTurn(hock: Hockeyist, target: Point, sector: Double = 0) = {
    ticksForTurn(hock, Math.max(0, Math.abs(hock.angleTo(target.x, target.y)) - sector))
  }

  def timeToArrivalDirect(hock: ModelUnit, target: Point, acceleration: Double = 0) = {
    val calculate = targetAfterCalculator(hock, acceleration)
    val targetDistance = hock.distanceTo(target)

    def adjust(estimate: Double, attempts: Int = 10): Double = {
//      println(s"$attempts: $estimate")
      if (attempts == 0) return estimate
      val dist = calculate(0, estimate.toLong).distanceTo(hock)
      val newEst = (estimate * targetDistance / dist).toLong
      if (Math.abs(newEst - estimate) < 0.5)
        newEst
      else
        adjust(newEst, attempts - 1)
    }

    adjust(50)
  }

  val angularSpeedK = 0.97
  val logAngularSpeedK = log(angularSpeedK)

  def angularSpeedAfter(as: Double, time: Long) = as * pow(angularSpeedK, time)

  def angleAfter(as: Double, time: Long) = {
    integ(0, time, as * pow(angularSpeedK, _) / logAngularSpeedK)
  }

  def velocityAfter(hock: ModelUnit, time: Long, acceleration: Double = 0) = {
    hock.velocity * pow(hock.brakeK, time) + acceleration*time
  }

  def targetAfterCalculator(hock: ModelUnit, acceleration: Double = 0, analyzeColision: Boolean = false) = {
    val realActor = hock.realActor
    val v0 = realActor.velocity
    val k = realActor.brakeK
    val logk = realActor.logBrakeK
    val fullAccel = /*realActor.realSpeedupToVelocityDirection * */ acceleration
    (timeFrom: Long, timeTo: Long) => {
      val distanceWoAcceleration = integ(timeFrom, timeTo, v0*pow(k,_)/logk)
      val accelDistance = if (fullAccel == 0) 0 else {
        integ(timeFrom, timeTo, t => fullAccel / k / logk * (pow(k, t) / logk - t))
      }
      val totalDistance = distanceWoAcceleration + accelDistance
      val targetPoint = realActor.velocityVector(totalDistance)(hock)
      if (analyzeColision) {
        val cw = getCollisionWithWall(hock, realActor.velocityVector)
        //very rude analysis
        if (cw != null && (cw.distanceTo(hock.point) < totalDistance)) {
          val restOfDistance = totalDistance - cw.distanceTo(hock.point)
          val mirrored = mirrorAt(realActor.velocityVector, cw)(restOfDistance / 4)
          mirrored(cw)
        }
        else {
          targetPoint
        }
      }
      else {
        targetPoint
      }
    }
  }

  def targetAfter(hock: ModelUnit, time: Long, acceleration: Boolean = false, analyzeColision: Boolean = true) = {
    targetAfterCalculator(hock, if (acceleration) hock.realActor.realSpeedupToVelocityDirection else 0, analyzeColision)(0, time)
  }

  lazy val g = WorldEx.game

  def mirrorAt(v: Geometry.Vector, p: Point) = {
    if (p.x == g.rinkLeft || p.x == g.rinkRight) {
      new Vector(-v.dx, v.dy)
    }
    else {
      new Vector(v.dx, -v.dy)
    }

  }

  def inRink(point: Point) = {
    point.x >= g.rinkLeft && point.x <= g.rinkRight && point.y >= g.rinkTop && point.y <= g.rinkBottom
  }

  def getCollisionWithWall(origin: Point, vector: Geometry.Vector): Point = {
    if (vector.length == 0) return null
    var xtop: Double = 0
    var xbottom: Double = 0
    var yleft: Double = 0
    var yright: Double = 0
    if (vector.dx != 0) {
      val vk = vector.dy / vector.dx
      xbottom = origin.x + (g.rinkBottom - origin.y) / vk
      xtop = origin.x + (g.rinkTop - origin.y) / vk
    }
    else {
      xbottom = origin.x
      xtop = origin.x
    }

    def checkMatch(point: Point) = {
      if (inRink(point) && vector.matchesDx(point.x - origin.x) && vector.matchesDy(point.y - origin.y)) Some(point) else None
    }

    if (vector.dy != 0) {
      val vk = vector.dx / vector.dy
      yleft = origin.y + (g.rinkLeft - origin.x) / vk
      yright = origin.y + (g.rinkRight - origin.x) / vk
    }
    else {
      yleft = origin.y
      yright = origin.y
    }
    List(
      new Point(xbottom, g.rinkBottom),
      new Point(xtop, g.rinkTop),
      new Point(g.rinkLeft, yleft),
      new Point(g.rinkRight, yright)).map(checkMatch).flatten.headOption.orNull

  }


  //1. развернуться, поехать
  //    60 тиков
  //    через, скажем, 30 тиков - пройдем 40 поинтов
  //2. поехать назад, развернуться на ходу, поехать
  //    20 тиков, разворот 44 тика - и уже доедем


}
