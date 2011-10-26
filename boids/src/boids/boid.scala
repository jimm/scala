package boids
import java.awt._

class Boid(_position:Point3D, _vector:Point3D) extends Thing {

  val DEFAULT_MAX_SPEED = 10.0

  var maxSpeed:Double = DEFAULT_MAX_SPEED
  var flock:Flock = _

  def getFlock = flock
  /** Called by a Flock when boid is added. */
  def setFlock(f:Flock) { flock = f }

  def velocity = vector
  def setVelocity(v:Point3D) { vector = v }

  def getMaxSpeed() = maxSpeed
  def setMaxSpeed(d:Double) { maxSpeed = d }

  override def move {
    moveTowardsFlockCenter
    avoidOthers
    matchOthersVelocities
    boundPosition
    limitSpeed
    super.move                          // Add velocity vector to position
  }

  def moveTowardsFlockCenter {
    val flockCenter:Point3D = flock.centerExcluding(this)
    // Division moves 1% of the way towards the center
    vector += (flockCenter - position) / 100.0
  }

  def avoidOthers {
    var c = new Point3D()
    for (b <- flock.boids if b != this) {
      val otherPos = b.position
      if (position.square_of_distance_to(otherPos) < 10000)
	c = c + position - otherPos
    }
    vector += c
  }

  def matchOthersVelocities {
    var vel = new Point3D()
    for (b <- flock.boids if b != this)
      vel += b.velocity
    vel /= flock.size - 1
    vel -= velocity
    vel /= 8.0
    vector += vel
  }

  def boundPositionVal(v:Double) =
    if (v < -1000) 30.0 else if (v > 1000) -30.0 else v

  def boundPosition {
    var v = new Point3D(boundPositionVal(position.x),
                        boundPositionVal(position.y),
                        boundPositionVal(position.z))
    vector += v
  }

  def limitSpeed {
    val speed = vector.length
    if (speed > maxSpeed)
      vector = vector / speed * maxSpeed
  }

  def erase(w:World) = drawWithColor(w, Color.white)

  def draw(w:World) = drawWithColor(w, Color.red)

  def drawWithColor(w:World, c:Color) {
    val size:Double = 4.0 + 3.0 * position.z / 1000.0
    val half:Double = size / 2.0

    w.setColor(c)
    w.drawLine(position, new Point3D(position.x + 1, position.y + 1, position.z + 1))
    w.fillCube(position.x - half, position.y - half, position.z - half, size, size, size)
  }

  override def toString = "Boid @ (position=" + position + ", vector=" + vector + ")"

}
