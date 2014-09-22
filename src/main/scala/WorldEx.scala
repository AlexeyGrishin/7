import model.{Unit => ModelUnit, _}
import StrictMath._

import scala.collection
import scala.collection.parallel.mutable

object WorldEx {
  import Geometry._

  var world: World = null
  var game: Game = null

  //var h: Hockeyist = null

  def init(world: World, game: Game, h: Hockeyist): Unit = {
    if (this.game == null) {
      println(game.hockeyistSpeedUpFactor)
      println(game.hockeyistSpeedDownFactor)
      println(game.hockeyistTurnAngleFactor)
      println(game.strikePowerBaseFactor)
      println(game.strikePowerGrowthFactor)
      println(game.struckPuckInitialSpeedFactor)
      println(game.goalieMaxSpeed)
    }
    this.world = world
    this.game = game
    //if (this.h == null || this.h.id == h.id) this.h = h
  }

  lazy val danger16= DangerArea.calculatePoints(world, game, puckSpeed = 16)
  lazy val dangerLeft16 = danger16.map(p => Seq(p.toLeft.toTop, p.toLeft.toBottom)).flatten
  lazy val dangerRight16 = danger16.map(p => Seq(p.toRight.toTop, p.toRight.toBottom)).flatten
  lazy val danger15= DangerArea.calculatePoints(world, game, puckSpeed = 15)
  lazy val dangerLeft15 = danger15.map(p => Seq(p.toLeft.toTop, p.toLeft.toBottom)).flatten
  lazy val dangerRight15 = danger15.map(p => Seq(p.toRight.toTop, p.toRight.toBottom)).flatten
  lazy val danger20= DangerArea.calculatePoints(world, game, puckSpeed = 20)
  lazy val dangerLeft20 = danger20.map(p => Seq(p.toLeft.toTop, p.toLeft.toBottom)).flatten
  lazy val dangerRight20 = danger20.map(p => Seq(p.toRight.toTop, p.toRight.toBottom)).flatten

  def isPuckOwnedBy(hock: Hockeyist) = world.puck.ownerHockeyistId.contains(hock.id)

  def isPuckCouldBeOwned(hock: Hockeyist): Boolean = {
    inRadius(hock, world.puck, game.stickLength) && Math.abs(hock.angleTo(world.puck)) <= game.stickSector / 2
  }

  def isPuckOwnedByOur = world.puck.ownerPlayerId.contains(world.myPlayer.get.id)

  lazy val leftZone = new PlayerZone(
    new Rectangle(new Point(0, 0), new Point(Geometry.middleX, Geometry.height)),
    new PointSpecZone(dangerLeft16),
    new PointSpecZone(dangerLeft20),
    new PointSpecZone(dangerLeft15),
    DangerArea.targetPoint(world, game).toLeft
  )

  lazy val rightZone = new PlayerZone(
    new Rectangle(new Point(Geometry.middleX, 0), new Point(Geometry.width, Geometry.height)),
    new PointSpecZone(dangerRight16),
    new PointSpecZone(dangerRight20),
    new PointSpecZone(dangerRight15),
    DangerArea.targetPoint(world, game).toRight
  )

  class PlayerZone(val half: Zone, val danger16: PointSpecZone, val danger20: PointSpecZone, val danger15: PointSpecZone, target: Point) {
    val targetTop: Point = target.toTop
    val targetBottom: Point = target.toBottom

    assert(half.includes(targetBottom))
    assert(half.includes(targetTop))
    danger15.selfcheck()
    danger20.selfcheck()
    danger15.selfcheck()
  }
  lazy val weOnRight = world.myPlayer.get.netRight > Geometry.middleX

  lazy val myZone = if (weOnRight) rightZone else leftZone
  lazy val enemyZone = if (!weOnRight) rightZone else leftZone




  implicit def unit2point(unit: ModelUnit): Point = new Point(unit.x, unit.y)
  def look(unit: ModelUnit): Vector = new Vector(cos(unit.angle), sin(unit.angle))
  implicit def unit2velocityVector(unit: ModelUnit): Vector = new Vector(unit.speedX, unit.speedY)

  class HockeystEx(var hockeyist: Hockeyist) {
    var move: Move = new Move()

    var moveVector: Vector = null
    var moveVector_target: Vector = null
    var moveVector_enemy: Vector = null
    var passTo: Point = null
    var startedTurning: Boolean = false

    def update(h: Hockeyist) = {
      hockeyist = h
      this
    }

    val isOur = hockeyist.playerId == world.myPlayer.get.id

    def realSpeedup(backward: Boolean = false) = if (backward) game.hockeyistSpeedDownFactor else game.hockeyistSpeedUpFactor //TODO: calculate with stamina/agility
    def realTurnspeed = game.hockeyistTurnAngleFactor

    def velocity = Math.hypot(hockeyist.speedX, hockeyist.speedY)

    var targetPoints: List[Point] = List()

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

    def canOwnPuck = {
      world.puck.distanceTo(hockeyist) < game.stickLength && Math.abs(hockeyist.angleTo(world.puck)) <= game.stickSector / 2
    }

    def canPunch(en: Hockeyist) = {
      en.distanceTo(hockeyist) < game.stickLength && Math.abs(hockeyist.angleTo(en)) <= game.stickSector / 2

    }

    def ownPuck = {
      world.puck.ownerHockeyistId.contains(hockeyist.id)
    }
  }


  val map = new collection.mutable.HashMap[Long, HockeystEx]

  implicit def h2ex(h: Hockeyist): HockeystEx = {
    map.getOrElseUpdate(h.id, new HockeystEx(h)).update(h)
  }


}
