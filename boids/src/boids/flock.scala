package boids
import java.awt.Graphics

class Flock {

  var members:List[Boid] = Nil

  def add(b:Boid) {
    members = b :: members
    b.setFlock(this)
  }

  def size = members.length

  def erase(w:World) = members.foreach(_.erase(w))

  def draw(w:World) = members.foreach(_.draw(w))

  def move = members.foreach(_.move)

  def boids = members

  /** Return distance between two boids' positions. */
  def distBetween(b1:Boid, b2:Boid) = b1.position distance_to b2.position

  /** Center of mass, excluding given boid. */
  def centerExcluding(boid:Boid):Point3D = {
    var p = new Point3D()
    for (b <- members if b != boid)
      p += b.position
    p / (members.length - 1)
}

}
