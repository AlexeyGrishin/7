import model.ActionType.{CancelStrike, Swing, Strike, TakePuck}
import model.HockeyistState.Swinging
import model.HockeyistType.Goalie
import model.{Unit => ModelUnit, _}

final class MyStrategy extends Strategy {

  import WorldEx._

  def measure(name: String)(action: => Unit): Unit = {
    val start = System.currentTimeMillis()
    action
    val diff = System.currentTimeMillis() - start
    //if (Logger.enabled) println(s"$name took $diff ms")
  }

  override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = measure("move") {
    //if (self.y == 460) return

    WorldEx(world, game, self)
    Trainer(world, game)
    Logger.doLog(world)
    self.role.move(self, world, game, move)

    /*
    1. скорости хокеиста и шайбы складываются. выгоднее лупить по ходу движения. например отрикошетив от борта. иначе - замах. можно рассчитать
          20*power + speed*cos(angle_look - angle_direction)
    5. зажимание в угол
     */

  //}
  if (self.state == Swinging && (!Seq(Strike, Swing, CancelStrike).contains(move.action) || move.speedUp != 0 || move.turn != 0)) {
    move.action = CancelStrike
  }

  self.move = move

}
}
