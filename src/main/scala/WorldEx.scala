import model.{Unit => ModelUnit, _}
import StrictMath._

import scala.collection
import scala.collection.parallel.mutable

object WorldEx {
  import Geometry._

  var world: World = null
  var game: Game = null

  //var h: Hockeyist = null

  def apply(world: World, game: Game, h: Hockeyist): Unit = {
    if (this.game == null) {
      println(game.hockeyistSpeedUpFactor)
      println(game.hockeyistSpeedDownFactor)
      println(game.hockeyistTurnAngleFactor)
      println(game.strikePowerBaseFactor)
      println(game.strikePowerGrowthFactor)
      println(game.struckPuckInitialSpeedFactor)
      println(game.stickLength)
      println(game.stickSector)
    }
    this.world = world
    this.game = game
    //if (this.h == null || this.h.id == h.id) this.h = h
  }

  lazy val dangersBySpeed: Map[Int, Array[Point]] = (15 to 26).map(
    s => s -> DangerArea.calculatePoints(world, game, puckSpeed = s)
  ).toMap


  def isPuckOwnedBy(hock: Hockeyist) = world.puck.ownerHockeyistId.contains(hock.id)

  def isPuckCouldBeOwned(hock: Hockeyist): Boolean = {
    inRadius(hock, world.puck, game.stickLength) && Math.abs(hock.angleTo(world.puck)) <= game.stickSector / 2
  }

  def isPuckOwnedByOur = world.puck.ownerPlayerId.contains(world.myPlayer.get.id)
  def isPuckOwnedByEnemy = world.puck.ownerPlayerId.contains(world.opponentPlayer.get.id)

  def puckOwner = world.hockeyists.find(_.ownPuck)

  val goalieR = 30.0

  lazy val leftZone = new PlayerZone(
    new Rectangle(new Point(0, 0), new Point(Geometry.middleX, Geometry.height)),
    dangersBySpeed.map(
      pair =>
        pair._1 -> new PointSpecZone(
          pair._2.toList.map(
            p => List[Point](p.toLeft.toTop, p.toLeft.toBottom)
          ).flatten
        )
    ),
    DangerArea.targetPoint(world, game).toLeft,
    new Rectangle(
      new Point(game.rinkLeft + goalieR*2.5, game.goalNetTop + game.goalNetHeight/2 - goalieR),
      new Point(game.rinkLeft + goalieR*4.5, game.goalNetTop + game.goalNetHeight/2 + goalieR)
    ),
    DangerArea.speedupZone1(world, game).toLeft,
    DangerArea.speedupZone2(world, game).toRight,
    DangerArea.startZone(world, game).toLeft
  )

  lazy val rightZone = new PlayerZone(
    new Rectangle(new Point(Geometry.middleX, 0), new Point(Geometry.width, Geometry.height)),
    dangersBySpeed.map(pair => pair._1 -> new PointSpecZone(pair._2.toList.map(p => Seq(p.toRight.toTop, p.toRight.toBottom)).flatten)),
    DangerArea.targetPoint(world, game).toRight,
    new Rectangle(
      new Point(game.rinkRight - goalieR*2.5, game.goalNetTop + game.goalNetHeight/2 - goalieR),
      new Point(game.rinkRight - goalieR*4.5, game.goalNetTop + game.goalNetHeight/2 + goalieR)
    ),
    DangerArea.speedupZone1(world, game).toRight,
    DangerArea.speedupZone2(world, game).toLeft,
    DangerArea.startZone(world, game).toRight

  )



  class PlayerZone(val half: Zone,
                   val dangerBySpeed: Map[Int, PointSpecZone],
                   target: Point,
                   val net: Rectangle,
                   speedupZone1: Rectangle,
                   speedupZone2: Rectangle,
                   val start: Rectangle
                    ) {

    def danger(speed: Int) = dangerBySpeed(speed)

    val targetTop: Point = target.toTop
    val targetBottom: Point = target.toBottom

    val speedupZone1Top = speedupZone1.toTop
    val speedupZone1Bottom = speedupZone1.toBottom
    val speedupZone2Top = speedupZone2.toTop
    val speedupZone2Bottom = speedupZone2.toBottom


    assert(half.includes(targetBottom))
    assert(half.includes(targetTop))

    val dx = signum(net.cornerPoints.head.x - Geometry.middleX)

    val defaultDangerZone = danger(20)
  }
  lazy val weOnRight = world.myPlayer.get.netRight > Geometry.middleX

  lazy val myZone = if (weOnRight) rightZone else leftZone
  lazy val enemyZone = if (!weOnRight) rightZone else leftZone

  implicit def unit2point(unit: ModelUnit): Point = new Point(unit.x, unit.y)
  def look(unit: ModelUnit): Vector = new Vector(cos(unit.angle), sin(unit.angle))
  implicit def unit2velocityVector(unit: ModelUnit): Vector = new Vector(unit.speedX, unit.speedY)

  val map = new collection.mutable.HashMap[Long, HockeystEx]

  implicit def h2ex(h: Hockeyist): HockeystEx = {
    map.getOrElseUpdate(h.id, new HockeystEx(h)).update(h)
  }

  class ModelUnitEx(m: ModelUnit) {
    def velocity = Math.hypot(m.speedX, m.speedY)
    def realSpeedup(backward: Boolean = false) = m match {
      case h: Hockeyist => h2ex(h).realSpeedup(backward)
      case _ => 0.0
    }
    def realSpeedupToVelocityDirection = m match {
      case h: Hockeyist => if (h.lookVector.normal_*(velocityVector) > 0.8) realSpeedup() else 0.0
      case _ => 0.0
    }
    val brakeK = m match {
      case h: Hockeyist => 1.0 - 1.0 / 50
      case _ => 1.0 - 1.0 / 1000
    }
    val logBrakeK = log(brakeK)
    def velocityVector = if (m.speedX != 0 || m.speedY != 0) new Vector(m.speedX, m.speedY) else m match {
      case h: Hockeyist => h.lookVector
      case _ => new Vector(0,0)
    }
    def point = new Point(m.x,m.y)

    def realActor: ModelUnitEx = m match {
      case p: Puck if p.ownerHockeyistId.isDefined => u2ex(world.hockeyists.find(_.ownPuck).get)
      case _ => this
    }

  }

  implicit def u2ex(m: ModelUnit) = new ModelUnitEx(m)

  class DoubleEx(d: Double) {
    def ~~(an: Double): Boolean = abs(d - an) < 1e-4
  }

  implicit def d2ex(d: Double) = new DoubleEx(d)
}
