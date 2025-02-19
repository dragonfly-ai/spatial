/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.dragonfly.spatial

import narr.*
import slash.vector.*

import scala.collection.mutable.ArrayBuffer
import scala.collection.{Iterator, mutable}
import scala.reflect.ClassTag

/*
This implementation of octree lacks:
  - removal method?
  - spatially ordered iterator?
 */

trait Octant {
  def center: Vec[3]
  def width: Double  // what's a better name for this?
  var size: Int = 0

  given V3CT: ClassTag[Vec[3]] = ClassTag(Vec.zeros[3].getClass)

  private val nCorner = Vec[3]( center.x - width, center.y - width, center.z - width )
  private val pCorner = Vec[3]( center.x + width, center.y + width, center.z + width )

  def intersects(v: Vec[3], radiusSquared: Double):Boolean = {
    var distSquared = 0.0
    if (v.x < nCorner.x) {
      val diff = v.x - nCorner.x
      distSquared += diff * diff
    } else if (v.x > pCorner.x) {
      val diff = v.x - pCorner.x
      distSquared += diff * diff
    }
    if (v.y < nCorner.y) {
      val diff = v.y - nCorner.y
      distSquared += diff * diff
    } else if (v.y > pCorner.y) {
      val diff = v.y - pCorner.y
      distSquared += diff * diff
    }
    if (v.z < nCorner.z) {
      val diff = v.z - nCorner.z
      distSquared += diff * diff
    } else if (v.z > pCorner.z) {
      val diff = v.z - pCorner.z
      distSquared += diff * diff
    }

    distSquared <= radiusSquared
  }

  def nearestNeighbor(queryPoint: Vec[3]): Option[Vec[3]]

  def insert(p: Vec[3]): Octant

  def radialQuery(p: Vec[3], radiusSquared: Double): Array[Vec[3]]

}

// Weights?
// removal?

class Octree[T](width: Double, center:Vec[3] = Vec[3](0.0, 0.0, 0.0), nodeCapacity:Int = 32, minDepth:Int = 0, depthMAX:Int =  10) extends Iterable[(Vec[3], T)] {

  if (depthMAX < minDepth) throw new Exception(s"maxDepth: $depthMAX must exceed minDepth : $minDepth")

  val map: mutable.HashMap[Vec[3], T] = mutable.HashMap[Vec[3], T]()

  private var root: Octant = if (minDepth == 1) {
    new LeafOctant(width, center, nodeCapacity, minDepth, depthMAX)
  } else {
    new MetaOctant(width, center, nodeCapacity, minDepth, depthMAX)
  }

  def insert(p: Vec[3], value: T): Option[T] = synchronized {
    root = root.insert(p)
    map.put(p, value)
  }

  def nearestNeighbor(queryPoint: Vec[3]): Option[(Vec[3], T)] = for {
    np <- root.nearestNeighbor(queryPoint)
    value <- map.get(np)
  } yield {
    (np, value)
  }

  def radialQuery(p: Vec[3], radius: Double): List[(Vec[3], T)] = {
    var matches = List[(Vec[3], T)]()

    for (k <- root.radialQuery(p, radius * radius)) {
      map.get(k) match {
        case Some(v: T) => matches = (k, v) :: matches
        case None =>
      }
      //println(matches)
    }
    //println(s"Returing from radialQuery($p, $radius)")
    matches
  }

  override def size: Int = root.size

  override def iterator: Iterator[(Vec[3], T)] = map.iterator
}

object Octant {

  def apply(
    factory:(Double,Vec[3],Int,Int,Int) => Octant,
    width: Double, center:Vec[3], nodeCapacity:Int, minDepth:Int, depthMAX:Int
  ): NArray[NArray[NArray[Octant]]] = {

    val childWidth:Double = width / 2.0

    val mnD:Int = Math.max(0, minDepth - 1)
    val dMX:Int = Math.max(0, depthMAX - 1)

    NArray[NArray[NArray[Octant]]] (
      NArray[NArray[Octant]]( // - X
        NArray[Octant](      // - Y
          factory(childWidth, Vec[3](center.x - childWidth, center.y - childWidth, center.z - childWidth), nodeCapacity, mnD, dMX), // -X-Y-Z -> 0
          factory(childWidth, Vec[3](center.x - childWidth, center.y - childWidth, center.z + childWidth), nodeCapacity, mnD, dMX)  // -X-Y+Z -> 1
        ),
        NArray[Octant](      // + Y
          factory(childWidth, Vec[3](center.x - childWidth, center.y + childWidth, center.z - childWidth), nodeCapacity, mnD, dMX), // -X+Y-Z -> 0
          factory(childWidth, Vec[3](center.x - childWidth, center.y + childWidth, center.z + childWidth), nodeCapacity, mnD, dMX)  // -X+Y+Z -> 1
        )
      ),
      NArray[NArray[Octant]]( // + X
        NArray[Octant](      // - Y
          factory(childWidth, Vec[3](center.x + childWidth, center.y - childWidth, center.z - childWidth), nodeCapacity, mnD, dMX), // +X-Y-Z -> 0
          factory(childWidth, Vec[3](center.x + childWidth, center.y - childWidth, center.z + childWidth), nodeCapacity, mnD, dMX)  // +X-Y+Z -> 1
        ),
        NArray[Octant](      // + Y
          factory(childWidth, Vec[3](center.x + childWidth, center.y + childWidth, center.z - childWidth), nodeCapacity, mnD, dMX), // +X+Y-Z -> 0
          factory(childWidth, Vec[3](center.x + childWidth, center.y + childWidth, center.z + childWidth), nodeCapacity, mnD, dMX)  // +X+Y+Z -> 1
        )
      )
    )
  }
}

class MetaOctant(override val width: Double, override val center:Vec[3], nodeCapacity:Int, minDepth:Int, depthMAX:Int) extends Octant {

  // Represent Octants in an array
  val nodes: NArray[NArray[NArray[Octant]]] = Octant(
    if (depthMAX <= 1) {
      (w: Double, c:Vec[3], _:Int, _:Int, _:Int) => new MaxDepthOctant(w, c)
    } else if (minDepth > 1) {
      (w: Double, c:Vec[3], nc:Int, mnD:Int, dMX:Int) => new MetaOctant(w, c, nc, mnD, dMX)
    } else {
      (w: Double, c:Vec[3], nc:Int, mnD:Int, dMX:Int) => new LeafOctant(w, c, nc, mnD, dMX)
    },
    width,
    center,
    nodeCapacity,
    minDepth,
    depthMAX
  )

  override def insert(p: Vec[3]): Octant = {
    val x = if (p.x - center.x < 0.0) 0 else 1
    val y = if (p.y - center.y < 0.0) 0 else 1
    val z = if (p.z - center.z < 0.0) 0 else 1

    val insertedNode = nodes(x)(y)(z).insert(p)
    nodes(x)(y)(z) = insertedNode
    size = size + 1
    this
  }

  override def nearestNeighbor(queryPoint: Vec[3]): Option[Vec[3]] = {
    val x = if (queryPoint.x - center.x < 0.0) 0 else 1
    val y = if (queryPoint.y - center.y < 0.0) 0 else 1
    val z = if (queryPoint.z - center.z < 0.0) 0 else 1

    var node = nodes(x)(y)(z)

    if (node.size > 0) node.nearestNeighbor(queryPoint) else {

      var nn:Option[Vec[3]] = None
      var nd: Double = Double.MaxValue

      for (xi <- 0 to 1; yi <- 0 to 1; zi <- 0 to 1) {
        if (xi != x || yi != y || zi != z) {
          node = nodes(xi)(yi)(zi)
          if (node.size > 0) {
            node.nearestNeighbor(queryPoint) match {
              case Some(candidate: Vec[3]) =>
                val cd: Double = queryPoint.euclideanDistanceSquaredTo(candidate)
                if (cd < nd) {
                  nd = cd
                  nn = Some(candidate)
                }
              case _ =>
            }
          }
        }
      }
      nn
    }
  }

  override def radialQuery(p: Vec[3], radiusSquared: Double): Array[Vec[3]] = {

    val matches = mutable.ArrayBuilder.make[Vec[3]](V3CT)

    for ( x <- 0 to 1; y <- 0 to 1; z <- 0 to 1 ) {
      if (nodes(x)(y)(z).intersects(p, radiusSquared)) {
        matches.addAll(nodes(x)(y)(z).radialQuery(p, radiusSquared))
      }
      println(matches)
    }

    matches.result()
  }

}

class LeafOctant[+T](override val width: Double, override val center:Vec[3], nodeCapacity:Int, minDepth:Int, depthMAX:Int) extends Octant {
  val points: NArray[Vec[3]] = NArray.fill[Vec[3]](nodeCapacity)(null.asInstanceOf[Vec[3]])

  def insert(p: Vec[3]): Octant = {
    if (size < points.length ) {
      points(size) = p
      size = size + 1
      this
    } else split().insert(p)
  }

  def split(): Octant = {

    val replacementNode = {
      if (depthMAX == 0) new MaxDepthOctant(width, center)
      else new MetaOctant(width, center, nodeCapacity, minDepth - 1, depthMAX - 1)
    }

    var p:Int = 0
    while (p < points.length) {
      replacementNode.insert(points(p))
      p += 1
    }

    replacementNode
  }

  override def radialQuery(p: Vec[3], radiusSquared: Double): Array[Vec[3]] = {
    val matches = mutable.ArrayBuilder.make[Vec[3]](V3CT)

    var pi: Int = 0
    while (pi < points.length) {
      val pC = points(pi)
      if (pC.euclideanDistanceSquaredTo(p) <= radiusSquared) matches.addOne(pC)
      pi += 1
    }

    matches.result()
  }

  override def nearestNeighbor(queryPoint: Vec[3]): Option[Vec[3]] = {
    var minDistSquared = Double.MaxValue
    var closestPoint: Option[Vec[3]] = None

    var pi: Int = 0
    while (pi < points.length) {
      val pC = points(pi)
      val dist = pC.euclideanDistanceSquaredTo(queryPoint)
      if (dist < minDistSquared) {
        minDistSquared = dist
        closestPoint = Some(pC)
      }
      pi += 1
    }

    closestPoint
  }

}

class MaxDepthOctant[+T](override val width: Double, override val center:Vec[3] = Vec[3](0.0, 0.0, 0.0)) extends Octant {
  val points: ArrayBuffer[Vec[3]] = new ArrayBuffer[Vec[3]]

  def insert(p: Vec[3]): Octant = {
    points.addOne(p)
    size = size + 1
    this
  }

  override def radialQuery(p: Vec[3], radiusSquared: Double): Array[Vec[3]] = {

    val matches = mutable.ArrayBuilder.make[Vec[3]](V3CT)

    for (pC <- points) {
      if (pC.euclideanDistanceSquaredTo(p) <= radiusSquared) matches.addOne(pC)
    }

    matches.result()
  }

  override def nearestNeighbor(queryPoint: Vec[3]): Option[Vec[3]] = {
    var minDistSquared = Double.MaxValue
    var closestPoint: Option[Vec[3]] = None

    for (pC <- points) {
      val dist = pC.euclideanDistanceSquaredTo(queryPoint)
      if (dist < minDistSquared) {
        minDistSquared = dist
        closestPoint = Some(pC)
      }
    }

    closestPoint
  }

}