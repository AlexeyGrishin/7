import model.ActionType.{CancelStrike, Swing, Strike, TakePuck}
import model.HockeyistState.{Active, Resting, Swinging}
import model.HockeyistType.Goalie
import model.{Unit => ModelUnit, _}

final class MyStrategy extends Strategy {

  import WorldEx._

  def measure(name: String)(action: => Unit): Unit = {
    val start = System.currentTimeMillis()
    action
    val diff = System.currentTimeMillis() - start
  }

  //собственно стратегия
  override def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = measure("move") {

    //заполняем глобальные переменные для быстрого доступа
    WorldEx(world, game, self)
    //перераспределяем роли
    Trainer(world, game)
    //логируем предыдущий ход
    Logger.doLog(world)
    //всякая отладка
    world.hockeyists.foreach(h => {
      Physics.timeToArrivalForStick(h, world.puck, true)
    })
    //действуем
    if (self.state != Resting)
      self.role.move(self, world, game, move)

  //охранное условие - чтобы не заклиниться в swinging
  if (self.state == Swinging && (!Seq(Strike, Swing, CancelStrike, ActionType.None).contains(move.action) || move.speedUp != 0 || move.turn != 0)) {
    move.action = CancelStrike
  }

  self.move = move

}
}
