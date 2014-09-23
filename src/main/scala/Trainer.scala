import Roles._
import model.{HockeyistType, World, Game}
import WorldEx._

object Trainer {
  val Enemy = -1
  val Noone = 0
  val We = 1
  case class GameState(puckOwner: Int, overtime: Boolean)

  var gameState: GameState = new GameState(-2, false)

  def getGameState(world: World, game: Game) = {
    GameState(world.puck.ownerPlayerId match {
      case None => Noone
      case Some(id) if id == world.myPlayer.get.id => We
      case _ => Enemy
    }, world.tick > game.tickCount - 300 && (world.players.map(_.goalCount).max == 0))
  }

  lazy val totalCount = WorldEx.world.hockeyists.count(_.isMoveableOur)

  def reassignRoles(world: World, game: Game): Unit = {
    totalCount match {
      case 2 => reassignRoles2(world, game)
      case _ => reassignRoles3(world, game)
    }
  }

  def reassignRoles2(world: World, game: Game): Unit = {
    val our = world.hockeyists.filter(_.isMoveableOur).toList
    val List(withPuck, withoutPuck) = our.sortBy(h => if (h.ownPuck) 1 else 2)
    val List(nearToNet, farFromNet) = our.sortBy(h => h.distanceTo(WorldEx.myZone.net.middle))
    gameState match {
      case GameState(We, true) =>
        withPuck.role = Roles.StrikeToNet
        withoutPuck.role = Roles.KickAsses
      case GameState(Enemy, true) =>
        nearToNet.role = Roles.DoDefence
        farFromNet.role = Roles.LookupForPuck
      case GameState(We, _) =>
        if (withPuck.role != Roles.DoDefence) {
          withPuck.role = Roles.MakeGoalAlone
          if (world.puck.onEnemySide)
            withoutPuck.role = Roles.KickAsses
        }
      case GameState(Enemy, _) =>
        nearToNet.role = Roles.DoDefence
        farFromNet.role = Roles.LookupForPuck
      case GameState(Noone, _) =>
        nearToNet.role = Roles.DoDefence
        farFromNet.role = Roles.LookupForPuck
    }
  }

  def reassignRoles3(world: World, game: Game): Unit = {
    val attacker = world.hockeyists.find(h => h.isOur && h.hokeyistType == HockeyistType.Forward).get
    val defencer = world.hockeyists.find(h => h.isOur && h.hokeyistType == HockeyistType.Defenceman).get
    val helper = world.hockeyists.find(h => h.isOur && h.hokeyistType == HockeyistType.Versatile).get
    val withPuck = world.hockeyists.find(h => h.isOur && h.ownPuck)
    val withoutPuck = world.hockeyists.find(h => h.isOur && !h.ownPuck)
    gameState match {
      case GameState(We, true) =>
        withPuck.get.role = Roles.StrikeToNet
        withoutPuck.foreach(_.role = Roles.KickAsses)
      case GameState(Enemy, true) =>
        attacker.role = Roles.LookupForPuck
        helper.role = Roles.KickAsses
        defencer.role = Roles.DoDefence
      case GameState(We, _) =>
        if (withPuck.get != defencer)
          withPuck.get.role = Roles.MakeGoalAlone
        withoutPuck.foreach(_.role = Roles.KickAsses)
        defencer.role = Roles.DoDefence
      case GameState(Enemy, _) =>
        attacker.role = Roles.LookupForPuck
        helper.role = Roles.KickAsses
        defencer.role = Roles.DoDefence
      case GameState(Noone, _) =>
        attacker.role = Roles.LookupForPuck
        helper.role = Roles.KickAsses
        defencer.role = Roles.KickAsses

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
