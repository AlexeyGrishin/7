import java.io.{OutputStreamWriter, FileOutputStream, FileWriter}
import java.nio.file.{StandardCopyOption, Paths, Files}
import java.text.SimpleDateFormat
import java.util.Date

import Geometry.Point
import model.ActionType.Pass
import model.{Puck, Move, Hockeyist, World}
import WorldEx._

object Logger {
  var enabled = false
  var name = new SimpleDateFormat("MMdd_HHmmss").format(new Date())

  val outputDir = "./visualizer/"

  def prepareOs() = {
    val os = new FileOutputStream(outputDir + name + ".html")
    Files.copy(Paths.get("./visualizer/render.html"), os)
    os.write("<script>".getBytes)
    os
  }

  lazy val rendererOs = prepareOs()
  lazy val renderer = new OutputStreamWriter(rendererOs)


  def render(puck: Puck): Unit = {
    renderer.write("\n{")
    val kls = new StringBuilder
    kls.append("puck ")
    kls.append(if (puck.ownerPlayerId.contains(WorldEx.world.myPlayer.get.id)) "owned" else "")
    render("name" -> "puck", first = true)
    render("x" -> puck.x)
    render("y" -> puck.y)
    render("vx" -> puck.speedX)
    render("vy" -> puck.speedY)
    render("type" -> "circle")
    render("radius" -> puck.radius)
    render("tag" -> puck.id)
    render("klass" -> kls.toString())
    renderer.write("\n,vectors: [")
    addVector("vel", puck.speedX, puck.speedY, first = true)
    renderer.write("]")

    renderer.write("\n}")
  }

  def addVector(name: String, dx: Double, dy: Double, first: Boolean = false): Unit = {
    if (!first) renderer.write(",")
    renderer.write("{")
    render("name" -> name, first = true)
    render("kls" -> name)
    render("dx" -> dx)
    render("dy" -> dy)
    renderer.write("}")
  }


  def render(hockeyist: Hockeyist): Unit = {
    renderer.write("\n{")
    val kls = new StringBuilder
    val isOur = hockeyist.playerId == WorldEx.world.myPlayer.get.id
    kls.append(if (isOur) "our_hock" else "enemy_hock")
    kls.append(" ")
    kls.append(hockeyist.state.toString)
    render("name" -> ((if (isOur) "Our " else "Enemy ") + hockeyist.hokeyistType.toString), first = true)
    render("x" -> hockeyist.x)
    render("y" -> hockeyist.y)
    render("vx" -> hockeyist.speedX)
    render("vy" -> hockeyist.speedY)
    render("type" -> "circle")
    render("radius" -> hockeyist.radius)
    render("tag" -> hockeyist.id)
    render("klass" -> kls.toString())
    renderer.write("\n,vectors: [")
    addVector("vel", hockeyist.speedX, hockeyist.speedY, first = true)
    val lookDx = Math.cos(hockeyist.angle) * (5 + hockeyist.radius)
    val lookDy = Math.sin(hockeyist.angle) * (5 + hockeyist.radius)
    addVector("look", lookDx, lookDy)
    if (hockeyist.moveVector != null) {
      addVector("move", hockeyist.moveVector.dx, hockeyist.moveVector.dy)
      addVector("move target", hockeyist.moveVector_target.dx, hockeyist.moveVector_target.dy)
      addVector("move enemy", hockeyist.moveVector_enemy.dx, hockeyist.moveVector_enemy.dy)
    }
    renderer.write("]")
    renderer.write("\n},")
  }

  def render(pair: (Any, Any), first: Boolean = false): Unit = {
    if (!first) renderer.write(",")
    renderer.write("\n")
    pair match {
      case (key, value: Double) => renderer.write(key.toString + ": " + value.toString + "")
      case (key, value: Int) => renderer.write(key.toString + ": " + value.toString + "")
      case (key, value) => renderer.write(key.toString + ": '" + value.toString + "'")
    }

  }

  var savedWorld: World = null

  def doLog(world: World): Unit = {
    if (!enabled) return
    //if (world.tick > 100) return
    if (savedWorld == null) {
      savedWorld = world
    }

    if (savedWorld.tick == world.tick) return
    logWorld(savedWorld)
    savedWorld = world
  }

  def renderTargetPoint(hock: Hockeyist, p: Point): Unit = {
    renderer.write(s"{type:'point', name:'target for ${hock.hokeyistType}', x: ${p.x}, y: ${p.y}, tag: ${hock.id}},")
  }

  private def logWorld(world: World): Unit = {

    renderer.write("\nadd({items: [")
    for (hock <- world.hockeyists) {
      render(hock)
      for (p <- hock.targetPoints) {
        renderTargetPoint(hock, p)
      }
    }
    if (world.tick == 0) {
      renderDangerAreas()
    }

    render(world.puck)
    renderer.write("], log: \"\"")
    for (hock <- world.hockeyists.toList.sortBy(_.hokeyistType.toString)) {
      if (hock.isOur)
        logTurn(hock, hock.move)
    }
    renderer.write("});")
    renderer.flush()
  }

  def renderDangerAreas(): Unit = {
    renderer.write("\n{type: 'area', name: 'danger', klass: 'enemy_danger', points: [")
    WorldEx.enemyZone.danger16.borderPoints.foreach(p => renderer.write(s"[${p.x}, ${p.y}],"))
    renderer.write("[]]")
    renderer.write("},")
    renderer.write("\n{type: 'area', name: 'danger', klass: 'our_danger', points: [")
    WorldEx.myZone.danger16.borderPoints.foreach(p => renderer.write(s"[${p.x}, ${p.y}],"))
    renderer.write("[]]")
    renderer.write("},")
    renderer.write(s"{type:'point', name:'target', 'klass': 'our_target', x:${WorldEx.enemyZone.targetTop.x}, y:${WorldEx.enemyZone.targetTop.y}},")
  }

  def logTurn(hock: Hockeyist, move: Move): Unit = {
    if (!enabled) return
    val tl =
      (f"""
        | ${hock.hokeyistType}%s
        |   Turn: ${move.turn}%.2f (${move.turn / Math.PI * 180}%.2f)
        |   Speedup: ${move.speedUp}%.2f
        |   Action: ${move.action}%s
      """.stripMargin + (move.action match {
        case Pass =>
          s"""
            |   Pass angle: ${move.passAngle}
            |   Pass power: ${move.passPower}
          """.stripMargin
        case _ => ""
      })).split("[\r\n]").mkString("\\n")

    renderer.write(" + \"" + tl + "\"")
    renderer.flush()
  }

  def close(): Unit = {
    if (!enabled) return
    //renderer.write("});")
    if (savedWorld != null) logWorld(savedWorld)
    renderer.write("</script>")
    renderer.flush()
    rendererOs.close()
    Files.copy(Paths.get(outputDir + name + ".html"), Paths.get(outputDir + "last.html"), StandardCopyOption.REPLACE_EXISTING)
  }

}
