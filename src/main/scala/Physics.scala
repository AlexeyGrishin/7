import WorldEx._
import Geometry.Point
import Geometry.Vector
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
  def integ(to: Double, from: Double, ifn: (Double) => Double) = ifn(to) - ifn(from)

  def timeToArrival(hock: ModelUnit, target: Point, estimate: Long, acceleration: Double = 0) = {
    //very slow...
    val calculate = targetAfterCalculator(hock, acceleration)
    val baseEst = estimate - 5
    val base = calculate(0, baseEst)
    (1 to 10)
      .map(d => (baseEst + d, calculate(0, baseEst + d)))
      .map(p => (p._1, target.distanceTo(p._2)))
      .sortBy(f=>f._2).head._1
  }

  val angularSpeedK = 0.97
  val logAngularSpeedK = log(angularSpeedK)

  def angularSpeedAfter(as: Double, time: Long) = as * pow(angularSpeedK, time)

  def angleAfter(as: Double, time: Long) = integ(0, time, as * pow(angularSpeedK, _) / logAngularSpeedK)

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
      val distanceWoAcceleration = integ(timeTo, timeFrom, v0*pow(k,_)/logk)
      val accelDistance = if (fullAccel == 0) 0 else {
        integ(timeTo, timeFrom, t => fullAccel / k / logk * (pow(k, t) / logk - t))
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
