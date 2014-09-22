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
    }, world.tick > game.tickCount - 300)
  }

  lazy val totalCount = WorldEx.world.hockeyists.count(_.isMoveableOur)

  def reassignRoles(world: World, game: Game): Unit = {
    totalCount match {
      case 2 => reassignRoles2(world, game)
      case _ => reassignRoles3(world, game)
    }
  }

  def reassignRoles2(world: World, game: Game): Unit = {
    val List(nearToPuck, farFromPuck) = world.hockeyists.filter(_.isMoveableOur).sortBy(_.distanceTo(world.puck)).toList
    val List(withPuck, withoutPuck) = List(nearToPuck, farFromPuck).sortBy(h => if (h.ownPuck) 1 else 2)
    gameState match {
      case GameState(We, true) =>
        //TODO: direct strike here
        withPuck.role = Roles.MakeGoalAlone
        withoutPuck.role = Roles.KickAsses
      case GameState(Enemy, true) =>
        //TODO: defence here
        nearToPuck.role = Roles.LookupForPuck
        farFromPuck.role = Roles.LookupForPuck
      case GameState(We, _) =>
        withPuck.role = Roles.MakeGoalAlone
        withoutPuck.role = Roles.KickAsses
      case GameState(Enemy, _) =>
        //TODO: defence here
        nearToPuck.role = Roles.LookupForPuck
        farFromPuck.role = Roles.LookupForPuck
      case GameState(Noone, _) =>
        nearToPuck.role = Roles.LookupForPuck
        farFromPuck.role = Roles.KickAsses
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
        //TODO: direct strike here
        withPuck.get.role = Roles.MakeGoalAlone
        withoutPuck.foreach(_.role = Roles.KickAsses)
      case GameState(Enemy, true) =>
        //TODO: defence here
        attacker.role = Roles.LookupForPuck
        helper.role = Roles.KickAsses
        defencer.role = Roles.KickAsses
      case GameState(We, _) =>
        withPuck.get.role = Roles.MakeGoalAlone
        withoutPuck.foreach(_.role = Roles.KickAsses)
      case GameState(Enemy, _) =>
        //TODO: defence here
        attacker.role = Roles.LookupForPuck
        helper.role = Roles.KickAsses
        defencer.role = Roles.KickAsses
      case GameState(Noone, _) =>
        attacker.role = Roles.LookupForPuck
        helper.role = Roles.KickAsses
        defencer.role = Roles.KickAsses

    }
  }

  def apply(world: World, game: Game): Unit = {
    val newState = getGameState(world, game)
    if (newState != gameState) {
      println(s"${world.tick} $gameState")
      gameState = newState
      reassignRoles(world, game)
    }
  }

}
