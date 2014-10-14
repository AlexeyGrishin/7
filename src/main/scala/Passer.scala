import WorldEx._
import Geometry._
import model.ActionType.Pass
import model.{Move, Hockeyist}

//попытка давать пасы
object Passer {

  lazy val areaSize = game.stickLength
  lazy val areas = (game.rinkLeft to (game.rinkRight, areaSize)).map(x => {
    (game.rinkTop to (game.rinkBottom, areaSize)).map(y => {
      new Rectangle(new Point(x,y), new Point(x+areaSize-1, y+areaSize-1))
    })
  }).flatten.toList

  var passTarget: Point = null

  def timeBeforePass(me: Hockeyist, target: Point) = {
    val timeToTurn = Physics.timeToTurn(me, target, game.passSector / 2)
    Math.max(timeToTurn, me.remainingCooldownTicks)
  }

  def doPass(me: Hockeyist, move: Move) = {
    passTarget = findIdealForPass(me, true)
    val timeLeft = (world.hockeyists.filter(_.isMoveableEnemy).map(h => Physics.timeToArrivalForStick(h, world.puck)) ++ Array(999.0)).min
    val timeToTurn = Math.min(timeLeft, timeBeforePass(me, passTarget))
    if (timeToTurn < 2 || !Mover.doTurn(me, passTarget, move)) {
      move.passAngle = me.angleTo(passTarget.x, passTarget.y)
      move.passPower = 1
      move.action = Pass
      true
    }
    else {
      false
    }

  }

  //идеальной точкой для паса является та, куда не доедут соперники, но доедут наши. работает кривовато, доделать уже не успел.
  def findIdealForPass(me: Hockeyist, log: Boolean = false): Point = {
    me.passZones = List()
    areas.map(area => {
      val distance = me.distanceTo(area.middle.x, area.middle.y)
      val puckDistance = world.puck.distanceTo(area.middle.x, area.middle.y)
      val angleToTurn = Math.max(0, Math.abs(me.angleTo(area.middle.x, area.middle.y)) - game.passSector/2)
      val passTime = puckDistance / 15

      val enemiesCanReachIt = world.hockeyists.filter(_.isMoveableEnemy).count(h => /*Physics.timeToArrivalForStick_enough(h, area.middle, passTime)*/ h.distanceTo(area.middle) < areaSize)
      val aliesCanReachIt = Math.max(1, world.hockeyists.filter(h => h.isMoveableOur && h.id != me.id).count(h => h.distanceTo(area.middle) < areaSize))

      val enemiesOnWay = world.hockeyists.filter(_.isMoveableEnemy).count(h => Physics.isEnemyOnWay(me, area.middle, h))

      val goesToOurNet = Roles.puckWillGoToOurNet(me.point -> area.middle)
      //val closeToMiddle = Math.abs(area.middle.x  - enemyZone.net.middle.x)

      var score = (Math.PI - angleToTurn) + aliesCanReachIt - enemiesCanReachIt / 2 - enemiesOnWay - (if (goesToOurNet) 100 else 0)// - closeToMiddle/500

      if (log) me.passZones = (area,
        s"""
           | distance $distance<br>
           | puck sitance $puckDistance<br>
           | angleToTurn $angleToTurn<br>
           | pass time $passTime<br>
           | enemies can reach $enemiesCanReachIt<br>
           | alies can reach $aliesCanReachIt<br>
           | enemes on way $enemiesOnWay<br>
           | goesToOurNet $goesToOurNet<br>
           | <br>
           | score $score
         """.stripMargin
        ) :: me.passZones
      (area, score)
    }).sortBy(_._2).reverse.head._1.middle
  }

}
