package ai.dragonfly.spatial

import ai.dragonfly.math.vector.Vector3

import scala.collection.{Iterator, mutable}
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/*
This implementation of octree lacks a removal method and needs a spatially relevant iterator.
 */

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

  def insert(p: Vector3): PointRegionOctreeNode[T]

  def radialQuery(p: Vector3, radiusSquared: Double): mutable.MutableList[Vector3]
}

// Discritized Lab Space Cardinality: 857623
// Weights?
// removal?

@JSExportTopLevel("ai.dragonfly.spatial.PointRegionOctree")
class PointRegionOctree[T](width: Double, center:Vector3 = Vector3(0.0, 0.0, 0.0), nodeCapacity:Int = 10, maxDepth:Int =  10) extends Iterable[(Vector3, T)] {

  @JSExport val map: mutable.HashMap[Vector3, T] = new mutable.HashMap[Vector3, T]()

  private var root: PointRegionOctreeNode[T] = new PROctreeMapLeafNode[T](width, center, nodeCapacity, maxDepth)

  @JSExport def insert(p: Vector3, value: T): Unit = synchronized {
    root = root.insert(p)
    map.put(p, value)
  }

  @JSExport def radialQuery(p: Vector3, radius: Double): mutable.MutableList[(Vector3, T)] = {
    val matches = mutable.MutableList[(Vector3, T)]()

    for (k <- root.radialQuery(p, radius * radius)) {
      map.get(k) match {
        case Some(value: T) => matches += ((k, value))
        case None =>
      }
    }

    matches
  }

  @JSExport override def size: Int = root.size

  @JSExport override def iterator: Iterator[(Vector3, T)] = map.iterator
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

  override def insert(p: Vector3): PointRegionOctreeNode[T] = {
    val x = if (p.x - center.x < 0.0) 0 else 1
    val y = if (p.y - center.y < 0.0) 0 else 1
    val z = if (p.z - center.z < 0.0) 0 else 1

    val insertedNode = nodes(x)(y)(z).insert(p)
    nodes(x)(y)(z) = insertedNode
    size = size + 1
    insertedNode
  }

  override def radialQuery(p: Vector3, radiusSquared: Double): mutable.MutableList[Vector3] = {

    val matches = mutable.MutableList[Vector3]()

    for ( x <- 0 until 2; y <- 0 until 2; z <- 0 until 2 ) {
      if (nodes(x)(y)(z).intersects(p, radiusSquared)) {
        matches ++= nodes(x)(y)(z).radialQuery(p, radiusSquared)
      }
    }

    matches
  }
}

class PROctreeMapLeafNode[T](override val width: Double, override val center:Vector3 = Vector3(0.0, 0.0, 0.0), nodeCapacity:Int = 10, maxDepth:Int =  10) extends PointRegionOctreeNode[T] {
  val points: Array[Vector3] = new Array[Vector3](nodeCapacity)
  override var size: Int = 0

  def insert(p: Vector3): PointRegionOctreeNode[T] = {
    if (size < points.length ) {
      points(size) = p
      size = size + 1
      this
    } else split().insert(p)
  }

  def split(): PointRegionOctreeNode[T] = {

    val replacementNode = {
      if (maxDepth == 0) new PROctreeMapMaxDepthNode[T](width, center)
      else new PROctreeMapMetaNode[T](width, center, nodeCapacity, maxDepth - 1)
    }

    for (p <- points) {
      replacementNode.insert(p)
    }
    replacementNode
  }

  override def radialQuery(p: Vector3, radiusSquared: Double): mutable.MutableList[Vector3] = {
    val matches = new mutable.MutableList[Vector3]()

    for (pC <- points) {
      if (pC.distanceSquaredTo(p) <= radiusSquared) matches += pC
    }

    matches
  }
}

class PROctreeMapMaxDepthNode[T](override val width: Double, override val center:Vector3 = Vector3(0.0, 0.0, 0.0)) extends PointRegionOctreeNode[T] {
  val points: mutable.MutableList[Vector3] = mutable.MutableList[Vector3]()
  override var size: Int = 0

  def insert(p: Vector3): PointRegionOctreeNode[T] = {
    points += p
    size = size + 1
    this
  }

  override def radialQuery(p: Vector3, radiusSquared: Double): mutable.MutableList[Vector3] = {
    var matches = mutable.MutableList[Vector3]()

    for (pC <- points) {
      if (pC.distanceSquaredTo(p) <= radiusSquared) matches += pC
    }

    matches
  }
}