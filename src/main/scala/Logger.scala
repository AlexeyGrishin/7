import java.io.{OutputStreamWriter, FileOutputStream, FileWriter}
import java.nio.file.{StandardCopyOption, Paths, Files}
import java.text.SimpleDateFormat
import java.util.Date

import Geometry.{Rectangle, Point}
import model.ActionType.{TakePuck, Pass}
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
    renderer.write(",table: [")
    renderTable("angle", puck.velocityVector.normal.toString)
    renderer.write("[null,null]]")
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
    if (hockeyist.inZone) kls.append(" in_zone")
    render("name" -> ((if (isOur) "Our " else "Enemy ") + hockeyist.hockeyistType.toString + " " + hockeyist.id + " " + hockeyist.statusStr), first = true)
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
      if (hockeyist.moveVector_target != null) addVector("move target", hockeyist.moveVector_target.dx, hockeyist.moveVector_target.dy)
      if (hockeyist.moveVector_enemy != null) addVector("move enemy", hockeyist.moveVector_enemy.dx, hockeyist.moveVector_enemy.dy)
    }
    hockeyist.targetVectors.foreach(v => addVector(v._1, v._2.dx, v._2.dy))
    renderer.write("]")
    renderer.write(",table: [")
    renderTable("cooldown", hockeyist.remainingCooldownTicks)
    renderTable("kickdown", hockeyist.remainingKnockdownTicks)
    renderTable("anglespeed", hockeyist.angularSpeed)
    renderTable("time to puck", hockeyist.timeToGetPuck)
    renderTable("time to puck stat", hockeyist.timeToGetPuckStat)
    renderTable("angle", hockeyist.angle)
    renderTable("dist to walls", Mover.distanceToWalls(hockeyist))
    if (hockeyist.moveVector_enemy != null) renderTable("enemy_vector", hockeyist.moveVector_enemy.length)
    renderer.write("[null,null]]")
    renderer.write("\n},")
    if (hockeyist.move.action == TakePuck) {
      renderer.write(s"{type: 'circle', name: 'stick radius', radius: ${game.stickLength}, x: ${hockeyist.x}, y: ${hockeyist.y}, klass: 'stick'},")
    }
  }

  def renderTable(key: String, value: String): Unit = {
    renderer.write(s"['$key', '$value'],")
  }

  def renderTable(key: String, value: Double): Unit = {
    renderer.write(s"['$key', $value],")
  }

  def renderTable(key: String, value: Int): Unit = {
    renderer.write(s"['$key', $value],")
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
    renderer.write(s"{type:'point', name:'target for ${hock.hockeyistType}', x: ${p.x}, y: ${p.y}, tag: ${hock.id}},")
  }

  private def logWorld(world: World): Unit = {

    renderer.write("\nadd({items: [")
    for (hock <- world.hockeyists) {
      render(hock)
      for (p <- hock.targetPoints) {
        renderTargetPoint(hock, p)
      }
      for (a <- hock.passZones) {
        renderArea("pass", a._1, a._2)
      }
    }
    if (world.tick == 0) {
      renderDangerAreas()
    }

    render(world.puck)
    renderer.write("], log: \"" + Trainer.gameState.toString + "\\n\\n\" ")
    for (hock <- world.hockeyists.toList.sortBy(_.id.toString)) {
      if (hock.isMoveableOur)
        logTurn(hock, hock.move)
    }
    renderer.write("});")
    renderer.flush()
  }

  def renderArea(name: String, area: Rectangle, table: String = null): Unit = {
    renderer.write(s"\n{type: 'area', name: '$name', klass: '$name', points: [")
    area.borderPoints.foreach(p => renderer.write(s"[${p.x}, ${p.y}],"))
    area.targetPoints.foreach(p => renderer.write(s"[${p.x}, ${p.y}],"))
    Array(area.middle).foreach(p => renderer.write(s"[${p.x}, ${p.y}],"))
    renderer.write("[]]")
    if (table != null) {
      renderer.write(s",table:[['info','${table.replaceAll("[\n\r]", "")}']]")
    }
    renderer.write("},")

  }

  def renderDangerAreas(): Unit = {
    renderer.write("\n{type: 'area', name: 'net', klass: 'my_net', points: [")
    WorldEx.myZone.net.cornerPoints.foreach(p => renderer.write(s"[${p.x}, ${p.y}],"))
    renderer.write("[]]")
    renderer.write("},")
    renderer.write("\n{type: 'area', name: 'danger', klass: 'enemy_danger', points: [")
    WorldEx.enemyZone.defaultDangerZone.borderPoints.foreach(p => renderer.write(s"[${p.x}, ${p.y}],"))
    WorldEx.enemyZone.defaultDangerZone.targetPoints.foreach(p => renderer.write(s"[${p.x}, ${p.y}],"))
    renderer.write("[]]")
    renderer.write("},")
    renderer.write("\n{type: 'area', name: 'danger', klass: 'our_danger', points: [")
    WorldEx.myZone.danger(20).borderPoints.foreach(p => renderer.write(s"[${p.x}, ${p.y}],"))
    renderer.write("[]]")
    renderer.write("},")
    renderArea("speedup1", WorldEx.myZone.speedupZone1Top)
    renderArea("speedup1", WorldEx.myZone.speedupZone1Bottom)
    renderArea("speedup2", WorldEx.myZone.speedupZone2Top)
    renderArea("speedup2", WorldEx.myZone.speedupZone2Bottom)
    renderArea("start", WorldEx.myZone.start)
    renderer.write(s"{type:'point', name:'target', 'klass': 'our_target', x:${WorldEx.enemyZone.targetTop.x}, y:${WorldEx.enemyZone.targetTop.y}},")
  }

  def logTurn(hock: Hockeyist, move: Move): Unit = {
    if (!enabled) return
    val tl =
      (f"""
        | ${hock.hockeyistType}%s ${hock.id}%d ${hock.statusStr}%s
        |   T:${move.turn}%.2f S:${move.speedUp}%.2f A:${move.action}%s
        |   ${hock.role.name}%s: ${hock.role.lastStatus}%s
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
