package ai.dragonfly.spatial

import slash.squareInPlace
import slash.vector.{Vec, VectorBounds}

trait Octant {

  val center: Vec[3]
  val extent: Double

  // infNorm: half side length, L∞ norm (max distance from center to face)
  lazy val infNorm: Double = extent / 2.0
  private lazy val nCorner = Vec[3](center.x - infNorm, center.y - infNorm, center.z - infNorm)
  private lazy val pCorner = Vec[3](center.x + infNorm, center.y + infNorm, center.z + infNorm)
  lazy val bounds: VectorBounds[3] = VectorBounds[3](nCorner, pCorner)
  //private lazy val boundingRadius = nCorner.euclideanDistanceTo(pCorner) / 2.0

  def size: Int

  // TODO: replace this method body with an implementation in the next version of slash
  def intersects(v: Vec[3], radiusSquared: Double): Boolean = {
    var distSquared = 0.0

    if (v.x < nCorner.x) distSquared += squareInPlace(v.x - nCorner.x)
    else if (v.x > pCorner.x) distSquared += squareInPlace(v.x - pCorner.x)

    if (v.y < nCorner.y) distSquared += squareInPlace(v.y - nCorner.y)
    else if (v.y > pCorner.y) distSquared += squareInPlace(v.y - pCorner.y)

    if (v.z < nCorner.z) distSquared += squareInPlace(v.z - nCorner.z)
    else if (v.z > pCorner.z) distSquared += squareInPlace(v.z - pCorner.z)

    distSquared <= radiusSquared
  }

  inline def encompasses(v: Vec[3]): Boolean = bounds.contains(v)

  inline def minDistanceSquaredTo(v: Vec[3]): Double = bounds.minEuclidianDistanceSquaredTo(v)

}
