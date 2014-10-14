import Roles._
import model.{Hockeyist, HockeyistType, World, Game}
import WorldEx._

//раздает роли в зависимости от состояния игры
//состояние описывается всего 3мя значениями. что мало, конечно
// - кто держит шайбу (мы/они/никто) - основной критерий
// - овертайм без вратарей или нет - без вратарей не мучаемся с атакой. включается за 300 тиков до овертайма
// - время после гола - просто едут всех гасить :)
object Trainer {
  val Enemy = -1
  val Noone = 0
  val We = 1
  case class GameState(puckOwner: Int, overtime: Boolean, justMissedOrScored: Boolean)

  var gameState: GameState = new GameState(-2, false, false)

  def getGameState(world: World, game: Game) = {
    GameState(world.puck.ownerPlayerId match {
      case None => Noone
      case Some(id) if id == world.myPlayer.get.id => We
      case _ => Enemy
    }, world.tick > game.tickCount - 300 && (world.players.map(_.goalCount).max == 0), world.myPlayer.get.justMissedGoal || world.myPlayer.get.justScoredGoal)
  }

  lazy val totalCount = WorldEx.world.hockeyists.count(_.isOur) - 1 //goalie

  def reassignRoles(world: World, game: Game): Unit = {
    totalCount match {
      case 2 => reassignRoles2(world, game)
      case 3 => reassignRoles3(world, game)
      case 6 => reassignRoles6(world, game)
    }
  }

  def reassignRoles2_temp(world: World, game: Game): Unit = {
    val our = world.hockeyists.filter(_.isMoveableOur).toList
    val List(withPuck, withoutPuck) = our.sortBy(h => if (h.ownPuck) 1 else 2)
    val List(nearToNet, farFromNet) = our.sortBy(h => h.distanceTo(WorldEx.myZone.net.middle))

    nearToNet.role = DoDefence
    farFromNet.role = Roles.FoolingAround
    //if (withPuck.ownPuck) withPuck.role = Roles.StrikeToNet else withPuck.role = LookupForPuck
    //withoutPuck.role = /*if (world.tick < 500) Roles.FoolingAround else*/ DoDefence
  }

  //для 2х хокеистов. 1 всегда защищает, 2й всегда нападает
  def reassignRoles2(world: World, game: Game): Unit = {
    val our = world.hockeyists.filter(_.isMoveableOur).toList
    val List(withPuck, withoutPuck) = our.sortBy(h => if (h.ownPuck) 1 else 2)
    val List(nearToNet, farFromNet) = our.sortBy(h => h.distanceTo(WorldEx.myZone.net.middle))

    gameState match {
      case GameState(_, _, true) =>
        our.foreach(_.role = KickAsses)
      case GameState(We, true, _) =>
        withPuck.role = Roles.StrikeToNet
        withoutPuck.role = Roles.KickAsses
      case GameState(Enemy, true, _) =>
        nearToNet.role = Roles.DoDefence
        farFromNet.role = Roles.PreventStrike
      case GameState(We, _, _) =>
        if (withPuck.role != Roles.DoDefence) {
          withPuck.role = Roles.MakeGoal2
          withoutPuck.role = Roles.DoDefence
        }
      case GameState(Enemy, _, _) =>
        nearToNet.role = Roles.DoDefence
        farFromNet.role = Roles.PreventStrike
      case GameState(Noone, _, _) =>
        nearToNet.role = Roles.DoDefence
        farFromNet.role = Roles.LookupForPuck
    }
  }

  //для шестерых - рандом, заглушка, чтобы стратегию принимали. не доделывал.
  def reassignRoles6(world: World, game: Game): Unit = {
    val all = world.hockeyists.filter(h => h.isMoveableOur).toList
    val List(attacker, defencer, helper) = all
    reassignRoles36(world, game, attacker, defencer, helper)
  }

  //для трех - защитник напдает, форвард защищает, третий на подхвате.
  //почему так: у защитника больше выносливость - сложнее отлупить клюшкой. скорость и меткость схожи. сила меньше, но
  //если смог разогнаться, то сила уже не роляет
  def reassignRoles3(world: World, game: Game): Unit = {
    val defencer = world.hockeyists.find(h => h.isOur && h.hockeyistType == HockeyistType.Forward).get
    val attacker = world.hockeyists.find(h => h.isOur && h.hockeyistType == HockeyistType.Defenceman).get
    val helper = world.hockeyists.find(h => h.isOur && h.hockeyistType == HockeyistType.Versatile).get
    reassignRoles36(world, game, attacker, defencer, helper)
  }

  //опять же 1 защищает, 1 забивает, и 1 на подхвате
  def reassignRoles36(world: World, game: Game, attacker: Hockeyist, defencer: Hockeyist, helper: Hockeyist): Unit = {
    val withPuck = world.hockeyists.find(h => h.isOur && h.ownPuck)
    val withoutPuck = world.hockeyists.filter(h => h.isOur && !h.ownPuck)
    gameState match {
      case GameState(We, true, _) =>
        withPuck.get.role = Roles.StrikeToNet
        withoutPuck.foreach(_.role = Roles.KickAsses)
      case GameState(Enemy, true, _) =>
        attacker.role = Roles.LookupForPuck
        helper.role = Roles.LookupForPuck
        defencer.role = Roles.DoDefence
      case GameState(We, _, _) =>
        if (withPuck.get != defencer) {
          withPuck.get.role = Roles.MakeGoal2
          withoutPuck.foreach(_.role = Roles.KickAsses)
          defencer.role = Roles.DoDefence
        }
        else {
          withoutPuck.foreach(_.role = Roles.LookupForPuck)
        }
      case GameState(Enemy, _, _) =>
        attacker.role = Roles.LookupForPuck
        helper.role = Roles.PreventStrike
        defencer.role = Roles.DoDefence
      case GameState(Noone, _, _) =>
        attacker.role = Roles.LookupForPuck
        helper.role = Roles.LookupForPuck
        defencer.role = Roles.DoDefence

    }
  }

  def apply(world: World, game: Game): Unit = {
    val newState = getGameState(world, game)
    if (newState != gameState) {
      gameState = newState
      reassignRoles(world, game)
    }
  }

  def temp(world: World, game: Game): Unit = {
    val one :: others = world.hockeyists.filter(_.isMoveableOur).toList.sortBy(_.id).reverse
    one.role = DoDefence
    others.foreach(h => h.role = if (h.ownPuck) StrikeToNet else LookupForPuck)
  }

}
