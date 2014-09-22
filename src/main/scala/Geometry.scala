import model.{Unit => ModelUnit}

object Geometry {
  import StrictMath._
  def toDeg(rad: Double): Double = {
    rad / Math.PI * 180
  }

  def toRad(deg: Double): Double = {
    deg / 180 * Math.PI
  }

  def pi(mp: Double): Double = {
    Math.PI * mp
  }

  def pow2(`val`: Double): Double = {
    `val` * `val`
  }

  def distance(unit1: ModelUnit, unit2: ModelUnit): Double = {
    hypot(unit1.x - unit2.x, unit1.y - unit2.y)
  }

  def inRadius(unit1: ModelUnit, unit2: ModelUnit, radius: Double): Boolean = {
    distance(unit1, unit2) <= radius
  }

  class Point(val x: Double, val y: Double) {

    def withMirrored = Seq(
      toLeft.toTop,
      toLeft.toBottom,
      toRight.toTop,
      toRight.toBottom
    )
    def toLeft = new Point(
      if (x < middleX) x else width - x,
      y
    )
    def toTop = new Point(
      x,
      if (y < middleY) y else middleY + (middleY - y)
    )
    def toBottom = new Point(
      x,
      if (y > middleY) y else middleY - (y - middleY)
    )
    def toRight = new Point(
      if (x > middleX) x else width - x,
      y
    )

    def isLeft = x < middleX
    def isRight = x >= middleX
    def isBottom = y >= middleY
    def isTop = y < middleY

    override def toString = s"($x,$y)"

    def ->(p:Point) = new Vector(this, p)
    def ->(v:Vector) = v(this)

    def distanceTo(p: Point) = Math.hypot(p.x - x, p.y - y)

  }

  class Vector(val dx: Double, val dy: Double) {
    def this(from: Point, to: Point) =
     this(to.x - from.x, to.y - from.y)
    lazy val length = Math.hypot(dx, dy)

    def *(sc: Double) = new Vector(dx*sc, dy*sc)
    def normal = new Vector(dx/length, dy/length)

    def +(vec: Vector) = new Vector(dx + vec.dx, dy + vec.dy)

    def apply(from: Point) = new Point(from.x + dx, from.y + dy)

    def apply(newlen: Double) = normal * newlen

    def reverse = new Vector(-dx, -dy)

    def ort = new Vector(dy, -dx)

    def /=(coef: Double) = apply(coef / length)

    override def toString = f"<$dx%.2f $dy%.2f = $length%.2f>"
  }

  trait Zone {
    def includes(p: Point): Boolean
  }

  class Rectangle(p1: Point, p2: Point) extends Zone {
    override def includes(p: Point): Boolean = {
      p.x >= p1.x && p.x <= p2.x && p.y >= p1.y && p.y <= p2.y
    }
  }

  class PointSpecZone(points: Traversable[Point]) extends Zone {
    private val map = points.groupBy(_.y).map(kv => kv._1 -> (kv._2.map(_.x).min, kv._2.map(_.x).max))
    val borderPointsPerY = map.toList.map(kv => (new Point(kv._2._1, kv._1), new Point(kv._2._2, kv._1)))
    val borderPoints = borderPointsPerY.map(kv => Seq(kv._1, kv._2)).flatten
    val yes: List[Double] = map.keys.toList.sortBy(f=>f)
    val topY = if (yes.isEmpty) 0 else yes.min
    val bottomY = if (yes.isEmpty) 0 else yes.max

    override def includes(p: Point): Boolean = includes(p, 0)


    def includes(p: Point, pad: Int): Boolean = {
      if (p.y < topY || p.y > bottomY) return false
      yes.find(f => Math.abs(f - p.y) < 15) match {
        case Some(y: Double) => p.x >= map(y)._1 + pad && p.x <= map(y)._2 - pad
        case _ => false
      }
    }

    def nearestTo(p: Point): Traversable[Point] = {
      val t = if (p.distanceTo(borderPointsPerY.head._1) > p.distanceTo(borderPointsPerY.head._2)) borderPointsPerY.map(_._1) else borderPointsPerY.map(_._2)
      //t.filter(p => p.y == topY || p.y == bottomY)
      t
    }

    def selfcheck(): Unit = {
      for (point <- points) {
        if(!includes(point)) {
          assert(false, "point was not included")
        }
      }
    }
  }

  lazy val width = WorldEx.game.worldWidth
  lazy val middleX = width / 2
  lazy val height = WorldEx.game.worldHeight
  lazy val middleY = (WorldEx.game.rinkBottom + WorldEx.game.rinkTop) / 2


}
