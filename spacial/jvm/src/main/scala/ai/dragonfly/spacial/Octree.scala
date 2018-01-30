package ai.dragonfly.spacial

import ai.dragonfly.math.vector.Vector3

import scala.collection.mutable

trait PointRegionOctreeNode[T] {
  def center: Vector3
  def width: Double
  var size: Int

  private val nCorner = Vector3( center.x - width, center.y - width, center.z - width )
  private val pCorner = Vector3( center.x + width, center.y + width, center.z + width )

  def intersects(p: Vector3, radiusSquared: Double):Boolean = {
    var distSquared = radiusSquared
    if (p.x < nCorner.x) {
      val diff = p.x - nCorner.x
      distSquared -= diff * diff
    } else if (p.x > pCorner.x) {
      val diff = p.x - pCorner.x
      distSquared -= diff * diff
    }
    if (p.y < nCorner.y) {
      val diff = p.y - nCorner.y
      distSquared -= diff * diff
    } else if (p.y > pCorner.y) {
      val diff = p.y - pCorner.y
      distSquared -= diff * diff
    }
    if (p.z < nCorner.z) {
      val diff = p.z - nCorner.z
      distSquared -= diff * diff
    } else if (p.z > pCorner.z) {
      val diff = p.z - pCorner.z
      distSquared -= diff * diff
    }

    distSquared > 0
  }

  def insert(p: Vector3, value: T): PointRegionOctreeNode[T]

  def radialQuery(p: Vector3, radiusSquared: Double): Option[mutable.MutableList[(Vector3, T)]]
}

// Discritized Lab Space Cardinality: 857623
// Is this a map or a set?

class PointRegionOctree[T](width: Double, center:Vector3 = Vector3(0.0, 0.0, 0.0), nodeCapacity:Int = 10, maxDepth:Int =  10) {

  private var root: PointRegionOctreeNode[T] = new PROctreeMapLeafNode[T](width, center, nodeCapacity, maxDepth)

  def insert(p: Vector3, value: T): Unit = synchronized { root = root.insert(p, value) }

  def radialQuery(p: Vector3, radius: Double): Option[mutable.MutableList[(Vector3, T)]] = root.radialQuery(p, radius * radius)

  def size: Int = root.size
}

class PROctreeMapMetaNode[T](override val width: Double, override val center:Vector3 = Vector3(0.0, 0.0, 0.0), nodeCapacity:Int = 10, maxDepth:Int =  10) extends PointRegionOctreeNode[T] {

  private val childWidth = width / 2.0
  override var size: Int = 0

  // Represent Octants in an array
  val nodes: Array[Array[Array[PointRegionOctreeNode[T]]]] = Array[Array[Array[PointRegionOctreeNode[T]]]] (
    Array[Array[PointRegionOctreeNode[T]]]( // - X
      Array[PointRegionOctreeNode[T]](      // - Y
        new PROctreeMapLeafNode[T](childWidth, Vector3(center.x - childWidth, center.y - childWidth, center.z - childWidth), nodeCapacity, maxDepth - 1), // -X-Y-Z -> 0
        new PROctreeMapLeafNode[T](childWidth, Vector3(center.x - childWidth, center.y - childWidth, center.z + childWidth), nodeCapacity, maxDepth - 1)  // -X-Y+Z -> 1
      ),
      Array[PointRegionOctreeNode[T]](      // + Y
        new PROctreeMapLeafNode[T](childWidth, Vector3(center.x - childWidth, center.y + childWidth, center.z - childWidth), nodeCapacity, maxDepth - 1), // -X+Y-Z -> 0
        new PROctreeMapLeafNode[T](childWidth, Vector3(center.x - childWidth, center.y + childWidth, center.z + childWidth), nodeCapacity, maxDepth - 1)  // -X+Y+Z -> 1
      )
    ),
    Array[Array[PointRegionOctreeNode[T]]]( // + X
      Array[PointRegionOctreeNode[T]](      // - Y
        new PROctreeMapLeafNode[T](childWidth, Vector3(center.x + childWidth, center.y - childWidth, center.z - childWidth), nodeCapacity, maxDepth - 1), // +X-Y-Z -> 0
        new PROctreeMapLeafNode[T](childWidth, Vector3(center.x + childWidth, center.y - childWidth, center.z + childWidth), nodeCapacity, maxDepth - 1)  // +X-Y+Z -> 1
      ),
      Array[PointRegionOctreeNode[T]](      // + Y
        new PROctreeMapLeafNode[T](childWidth, Vector3(center.x + childWidth, center.y + childWidth, center.z - childWidth), nodeCapacity, maxDepth - 1), // +X+Y-Z -> 0
        new PROctreeMapLeafNode[T](childWidth, Vector3(center.x + childWidth, center.y + childWidth, center.z + childWidth), nodeCapacity, maxDepth - 1)  // +X+Y+Z -> 1
      )
    )
  )

  override def insert(p: Vector3, value: T): PointRegionOctreeNode[T] = {
    val x = if (p.x - center.x < 0.0) 0 else 1
    val y = if (p.y - center.y < 0.0) 0 else 1
    val z = if (p.z - center.z < 0.0) 0 else 1

    val insertedNode = nodes(x)(y)(z).insert(p, value)
    nodes(x)(y)(z) = insertedNode
    size = size + 1
    insertedNode
  }


  override def radialQuery(p: Vector3, radiusSquared: Double): Option[mutable.MutableList[(Vector3, T)]] = {
    var matched = false
    lazy val matches = { matched = true; mutable.MutableList[(Vector3, T)]() }

    for ( x <- 0 until 2; y <- 0 until 2; z <- 0 until 2 ) {
      if (nodes(x)(y)(z).intersects(p, radiusSquared)) {
        nodes(x)(y)(z).radialQuery(p, radiusSquared) match {
          case Some(ms: mutable.MutableList[(Vector3, T)]) => matches ++= ms
          case _ =>
        }
      }
    }

    if (matched) Some(matches) else None
  }
}

class PROctreeMapLeafNode[T](override val width: Double, override val center:Vector3 = Vector3(0.0, 0.0, 0.0), nodeCapacity:Int = 10, maxDepth:Int =  10) extends PointRegionOctreeNode[T] {
  val pointValues: Array[(Vector3, T)] = new Array[(Vector3, T)](nodeCapacity)
  override var size: Int = 0

  def insert(p: Vector3, value: T): PointRegionOctreeNode[T] = {
    if (size < pointValues.length ) {
      pointValues(size) = (p, value)
      size = size + 1
      this
    } else split().insert(p, value)
  }

  def split(): PointRegionOctreeNode[T] = {

    val replacementNode = {
      if (maxDepth == 0) new PROctreeMapMaxDepthNode[T](width, center)
      else new PROctreeMapMetaNode[T](width, center, nodeCapacity, maxDepth - 1)
    }

    for (pv <- pointValues) {
      replacementNode.insert(pv._1, pv._2)
    }
    replacementNode
  }

  override def radialQuery(p: Vector3, radiusSquared: Double): Option[mutable.MutableList[(Vector3, T)]] = {
    var matched = false
    lazy val matches = { matched = true; new mutable.MutableList[(Vector3, T)]() }

    for (pv <- pointValues) {
      if (pv._1.distanceSquaredTo(p) <= radiusSquared) matches += pv
    }

    if (matched) Some(matches) else None
  }
}

class PROctreeMapMaxDepthNode[T](override val width: Double, override val center:Vector3 = Vector3(0.0, 0.0, 0.0)) extends PointRegionOctreeNode[T] {
  val pointValues: mutable.MutableList[(Vector3, T)] = mutable.MutableList[(Vector3, T)]()
  override var size: Int = 0

  def insert(p: Vector3, value: T): PointRegionOctreeNode[T] = {
    pointValues += ((p, value))
    size = size + 1
    this
  }

  override def radialQuery(p: Vector3, radiusSquared: Double): Option[mutable.MutableList[(Vector3, T)]] = {
    var matched = false
    lazy val matches = { matched = true; mutable.MutableList[(Vector3, T)]() }

    for (pv <- pointValues) {
      if (pv._1.distanceSquaredTo(p) <= radiusSquared) matches += pv
    }

    if (matched) Some(matches) else None
  }
}