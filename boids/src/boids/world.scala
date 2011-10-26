package boids
import java.awt._
import java.awt.event._

object BoidRunner {
  def main(args:Array[String]) {
    new Thread(new World).run()
  }
}

class World extends Frame with Runnable {

  val SLEEPTIME_MILLIS = 25L
  val WINDOW_HEIGHT = 600
  val WINDOW_WIDTH = 600

  var flock = new Flock
  var camera = new Camera(new Point3D(-500.0, -30.0, 20.0), new Point3D(500.0, 30.0, -20.0))
  var canvas = new Canvas()
  var running = false

  setTitle("Boids")
  add(canvas)
  setSize(WINDOW_HEIGHT, WINDOW_WIDTH)

  val rand = new java.util.Random()
  for (i <- 1 to 10) {
    val pos = new Point3D(rand.nextDouble * 100, rand.nextDouble * 100, rand.nextDouble * 100)
    flock.add(new Boid(pos, Point3D.ORIGIN))
  }

  addWindowListener(new WindowAdapter() {
    override def windowClosing(e:WindowEvent) { running = false }
  })

  setVisible(true)

  def run {
    running = true
    while (running) {
      erase
      drawFloor
      flock.erase(this)
      flock.move
      flock.draw(this)
      try {
        Thread.sleep(SLEEPTIME_MILLIS)
      }
      catch {
        case e: InterruptedException => running = false
      }
    }
    System.exit(0)
  }

  def erase {
    setColor(Color.white)
    graphics.fillRect(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
  }

  def drawFloor {
    for (z <- -1000 to 1000 if (z % 50) == 0)
      drawLine(-1000, 0, z, 1000, 0, z)
    for (x <- -1000 to 1000 if (x % 50) == 0)
      drawLine(x, 0, -1000, x, 0, 1000)
  }

  def drawLine(p0:Point3D, p1:Point3D) {
    drawLine(p0.x, p0.y, p0.z, p1.x, p1.y, p1.z)
  }

  def drawLine(x0:Double, y0:Double, z0:Double, x1:Double, y1:Double, z1:Double) {
    val p0 = mapTo2D(x0, y0, z0)
    val p1 = mapTo2D(x1, y1, z1)
    graphics.drawLine(p0.x, p0.y, p1.x, p1.y)
  }

  def fillCube(x0:Double, y0:Double, z0:Double, width:Double, depth:Double, height:Double) {
    val p0 = mapTo2D(x0, y0, z0)
    val p1 = mapTo2D(x0 + width, y0 + depth, z0 + height)
    graphics.fillRect(p0.x, p0.y, p1.x - p0.x, p1.y - p0.y)
  }

  /**
   * From http://www.geocities.com/SiliconValley/Horizon/6933/3d.html
   */
  def mapTo2D(x:Double, y:Double, z:Double):Point = {
    val distOverZ = if (z == 0) 0.0 else camera.position.distance_to(Point3D.ORIGIN)
    new Point(WINDOW_WIDTH  / 2 + (x / distOverZ).toInt,
              WINDOW_HEIGHT / 2 + (y / distOverZ).toInt)
  }

  def setColor(c:Color) = graphics.setColor(c)

  def graphics = canvas.getGraphics
}
