import java.lang.StrictMath._

import Geometry.{Zone, Rectangle, Point, Vector}
import Mover.StrikePoint
import model.ActionType.{Pass, Strike, TakePuck, Swing}
import model.HockeyistState.{Resting, Swinging, KnockedDown}
import model.HockeyistType.Goalie
import model.{ActionType, Move, Hockeyist}

class HockeystEx(var hockeyist: Hockeyist) {

  import WorldEx._

  var move: Move = new Move()
  var _role: Role = null

  def role = _role
  def role_=(newRole: Role): Unit = {
    if (newRole.name.equals(Option(_role).map(_.name).getOrElse("null"))) return
    _role = newRole
    moveVector = null
    moveVector_enemy = null
    moveVector_target = null
    passTo = null
    passFrom = null
    passFromArrival = -1
    startedTurning = false
    justAdjusted = false
    targetVectors = List()
    targetPoints = List()
    inZone = false
    nextZone = null
    strikePoint = null
    passZones = List()
    Passer.passTarget = null
  }

  var moveVector: Vector = null
  var moveVector_target: Vector = null
  var moveVector_enemy: Vector = null
  var targetVectors: List[(String, Vector)] = List()
  var passTo: Point = null
  var passFrom: Point = null
  var passFromArrival: Long = 0
  var startedTurning: Boolean = false
  var inZone: Boolean = false
  var justAdjusted: Boolean = false

  var passZones: List[(Rectangle, String)] = List()

  var nextZone: Zone = null
  var strikePoint: StrikePoint = null

  //enemy
  var timeToGetPuck: Double = 0
  var timeToGetPuckStat: String = ""

  def update(h: Hockeyist) = {
    hockeyist = h
    this
  }

  val isOur = hockeyist.playerId == world.myPlayer.get.id

  val isMoveableOur = hockeyist.playerId == world.myPlayer.get.id && hockeyist.hockeyistType != Goalie && hockeyist.state != Resting

  val isMoveableEnemy =
    (hockeyist.playerId == world.opponentPlayer.get.id &&
      hockeyist.hockeyistType != Goalie &&
      hockeyist.state != Resting &&
      hockeyist.state != KnockedDown)// || (role == Roles.FoolingAround)

  def realSpeedup(backward: Boolean = false) = if (backward) game.hockeyistSpeedDownFactor*agilityCoef else game.hockeyistSpeedUpFactor*agilityCoef
  def realTurnspeed = game.hockeyistTurnAngleFactor*agilityCoef

  def velocity = Math.hypot(hockeyist.speedX, hockeyist.speedY)

  var targetPoints: List[Point] = List()

  def <<(rect: Rectangle) = {
    if (WorldEx.weOnRight) hockeyist.x > rect.p2.x else hockeyist.x < rect.p1.x
  }
  def >>(rect: Rectangle) = {
    if (WorldEx.weOnRight) hockeyist.x < rect.p1.x else hockeyist.x > rect.p2.x
  }

  def angleTo(from: Point, to: Point): Double = {
    val absoluteAngleTo: Double = atan2(to.y - from.y, to.x - from.x)
    var relativeAngleTo: Double = absoluteAngleTo - hockeyist.angle

    while (relativeAngleTo > PI) {
      relativeAngleTo -= 2.0D * PI
    }

    while (relativeAngleTo < -PI) {
      relativeAngleTo += 2.0D * PI
    }

    relativeAngleTo
  }

  def lookVector = new Vector(cos(hockeyist.angle), sin(hockeyist.angle))

  def canOwnPuck = {
    hockeyist.remainingCooldownTicks == 0 && world.puck.distanceTo(hockeyist) < game.stickLength && Math.abs(hockeyist.angleTo(world.puck)) <= game.stickSector / 2
  }



  def canPunch(en: Hockeyist) = {
    hockeyist.remainingCooldownTicks == 0 && en.distanceTo(hockeyist) < game.stickLength && Math.abs(hockeyist.angleTo(en)) <= game.stickSector / 2
  }

  def statusStr = {
    val cleft = if (move.turn < 0) "&lt;" else " "
    val cright = if (move.turn > 0) ">" else " "
    val fwd = if (move.speedUp > 0) "^" else if (move.speedUp < 0) "v" else " "
    val status = (hockeyist.state, move.action, hockeyist.remainingCooldownTicks) match {
      case (KnockedDown, _, _) => ">_&lt;"
      case (_, ActionType.None, cd) if cd > 0 => "-_-"
      case (_, Strike, _) => "@_@"
      case (Swinging, _, _) => "/_/"
      case (_, Swing, _) => "/_/"
      case (_, TakePuck, _) => "^_^"
      case (_, Pass, _) => ">_>"
      case _ => "o_o"
    }
    s"$cleft$fwd$cright $status"
  }

  def ownPuck = {
    world.puck.ownerHockeyistId.contains(hockeyist.id)
  }

  def staminaCoeff = hockeyist.stamina / game.hockeyistMaxStamina

  def effective(v: => Double) = 0.75*v + 0.25*v*staminaCoeff

  def strength = effective(hockeyist.strength)
  def endurance = effective(hockeyist.endurance)
  def dexterity = effective(hockeyist.dexterity)
  def agility = effective(hockeyist.agility)

  def strengthCoef = strength / 100
  def agilityCoef = agility / 100

  def puckSpeedAfterStrike(swingTicks: Double, vel: Double = velocity) = 20 * (0.75 + 0.25*swingTicks / game.maxEffectiveSwingTicks) * strengthCoef + vel*(lookVector normal_* hockeyist.velocityVector)

  //dexterity - хорошо для того кто пинает в ворота

  //dexterity/agility - прибавляет процент для перехвата шайбы

}