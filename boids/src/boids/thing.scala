package boids

class Thing(_position:Point3D, _vector:Point3D) {

  var position = _position
  var vector = _vector

  def this() = this(Point3D.ORIGIN, Point3D.ORIGIN)

  def move = position = position + vector

}
