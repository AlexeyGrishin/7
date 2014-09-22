import WorldEx._
import Geometry.Point
import model.Hockeyist
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

  val Kh = 1.0 - 1.0 / 50
  val lnKh = log(Kh)
  val Kp = 1.0 - 1.0 / 1000

  def integ(to: Double, ifn: (Double) => Double) = ifn(to) - ifn(0)

  def targetAfter(hock: Hockeyist, time: Long, acceleration: Double = 0) = {
    val v0 = hock.velocity
    val distanceWoAcceleration = integ(time, v0*pow(Kh,_)/lnKh)
    val fullAccel = hock.realSpeedup() * acceleration
    val accelDistance = if (acceleration == 0) 0 else {
      integ(time, t => fullAccel / Kh / lnKh * (pow(Kh, t) / lnKh - t))
    }
    unit2velocityVector(hock)(distanceWoAcceleration + accelDistance)(hock)
  }


  //1. развернуться, поехать
  //    60 тиков
  //    через, скажем, 30 тиков - пройдем 40 поинтов
  //2. поехать назад, развернуться на ходу, поехать
  //    20 тиков, разворот 44 тика - и уже доедем


}
