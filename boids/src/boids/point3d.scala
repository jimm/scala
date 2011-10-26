package boids

case class Point3D(x:Double, y:Double, z:Double) {

  def this() = this(0, 0, 0)

  def +(p:Point3D) = Point3D(x + p.x, y + p.y, z + p.z)
  def -(p:Point3D) = Point3D(x - p.x, y - p.y, z - p.z)
  def *(p:Point3D) = Point3D(x * p.x, y * p.y, z * p.z)
  def /(p:Point3D) = Point3D(x / p.x, y / p.y, z / p.z)

  def +(d:Double) = Point3D(x + d, y + d, z + d)
  def -(d:Double) = Point3D(x - d, y - d, z - d)
  def *(d:Double) = Point3D(x * d, y * d, z * d)
  def /(d:Double) = Point3D(x / d, y / d, z / d)

  def square_of_distance_to(p:Point3D) = {
    val dx = x - p.x
    val dy = x - p.y
    val dz = x - p.z
    dx * dx + dy * dy + dz * dz
  }
  def distance_to(p:Point3D) = Math.sqrt(square_of_distance_to(p))
  def length = distance_to(Point3D.ORIGIN)

  override def toString = "Point3D(" + x + ", " + y + ", " + z + ")"

}

object Point3D {
  val _ORIGIN = new Point3D(0, 0, 0)
  def ORIGIN =_ORIGIN
}
