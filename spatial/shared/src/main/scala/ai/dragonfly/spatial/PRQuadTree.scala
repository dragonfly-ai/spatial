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
import slash.squareInPlace
import slash.vector.*

import scala.collection.mutable

class PRQuadTree(extent: Double, center: Vec[2] = Vec[2](0.0, 0.0), maxNodeCapacity: Int = 64) {

  private var root: PRQuadrant = new LeafPRQuadrant(center, extent, maxNodeCapacity)

  def insert(v: Vec[2]): Boolean = synchronized {
    if (encompasses(v)) {
      val s = root.size
      root = root.insert(v)
      s + 1 == root.size
    } else false
  }

  inline def encompasses(qv: Vec[2]): Boolean = root.encompasses(qv)

  def nearestNeighbor(qv: Vec[2]): Vec[2] = {
    if (this.size < 1) throw IllegalArgumentException(
      s"knn can't find a nearest neighbor in PROctree of size: ${this.size}."
    )

    var nn: Vec[2] = null.asInstanceOf[Vec[2]]
    var minDistSquared: Double = Double.MaxValue

    def search(node: PRQuadrant): Unit = node match {
      case lo: LeafPRQuadrant =>
        var pi = 0
        while (pi < lo.points.size) {
          val p = lo.points(pi)
          val dstSq = qv.euclideanDistanceSquaredTo(p) //(v - qv).magnitude
          if (dstSq < minDistSquared) {
            minDistSquared = dstSq
            nn = p
          }
          pi = pi + 1
        }
      case mo: MetaPRQuadrant =>
        // Check distance to node boundary
        val closestDistSqrd = mo.minDistanceSquaredTo(qv)
        if (nn == null.asInstanceOf[Vec[2]] || closestDistSqrd < minDistSquared) mo.foreachNode(search)
    }

    search(root)
    nn
  }

  // K-Nearest Neighbor Search
  def knn(qv: Vec[2], k: Int): NArray[Vec[2]] = {
    if (this.size < k) throw IllegalArgumentException(
      s"knn can't find $k nearest neighbors in PRQuadTree of size: ${this.size}."
    )

    case class Candidate(point: Vec[2], distanceSquared: Double)
    given ord: Ordering[Candidate] = Ordering.by(_.distanceSquared) // Min-heap (closest first)
    val pq = mutable.PriorityQueue.empty[Candidate](ord)

    def search(node: PRQuadrant): Unit = node match {
      case lq: LeafPRQuadrant =>
        var pi = 0
        while (pi < lq.points.size) {
          val p = lq.points(pi)
          val dstSq = qv.euclideanDistanceSquaredTo(p)
          if (pq.size < k) pq.enqueue(Candidate(p, dstSq))
          else if (dstSq < pq.head.distanceSquared) {
            pq.dequeue()
            pq.enqueue(Candidate(p, dstSq))
          }
          pi = pi + 1
        }
      case mq: MetaPRQuadrant =>
        val closestDist = mq.minDistanceSquaredTo(qv)
        if (pq.size < k || closestDist < pq.head.distanceSquared) mq.foreachNode(search)
    }

    search(root)

    NArray.tabulate[Vec[2]](pq.size)(_ => pq.dequeue().point)
  }

  def radialQuery(pq: Vec[2], radius: Double): NArray[Vec[2]] = root.radialQuery(pq, squareInPlace(radius))

  def size: Int = root.size

  def bounds: VectorBounds[2] = root.bounds
}

trait PRQuadrant extends Quadrant {

  def insert(v: Vec[2]): PRQuadrant

  def radialQuery(qv: Vec[2], radiusSquared: Double): NArray[Vec[2]]
}

class MetaPRQuadrant(override val center: Vec[2], override val extent: Double, maxNodeCapacity: Int = 64) extends PRQuadrant {

  var s: Int = 0

  // Represent Quadrants in a 2x2 array
  val nodes: NArray[NArray[PRQuadrant]] = {
    val childInfNorm: Double = infNorm / 2.0
    val childExtent: Double = infNorm

    NArray[NArray[PRQuadrant]](
      NArray[PRQuadrant]( // -X
        LeafPRQuadrant(Vec[2](center.x - childInfNorm, center.y - childInfNorm), childExtent, maxNodeCapacity), // SW: -X-Y
        LeafPRQuadrant(Vec[2](center.x - childInfNorm, center.y + childInfNorm), childExtent, maxNodeCapacity)  // NW: -X+Y
      ),
      NArray[PRQuadrant]( // +X
        LeafPRQuadrant(Vec[2](center.x + childInfNorm, center.y - childInfNorm), childExtent, maxNodeCapacity), // SE: +X-Y
        LeafPRQuadrant(Vec[2](center.x + childInfNorm, center.y + childInfNorm), childExtent, maxNodeCapacity)  // NE: +X+Y
      )
    )
  }

  def foreachNode(f: PRQuadrant => Any): Unit = {
    var xi = 0
    while (xi < 2) {
      var yi = 0
      while (yi < 2) {
        f(nodes(xi)(yi))
        yi = yi + 1
      }
      xi = xi + 1
    }
  }

  override inline def size: Int = s

  override def insert(v: Vec[2]): PRQuadrant = {
    if (this.encompasses(v)) {
      val x = if (v.x - center.x < 0.0) 0 else 1
      val y = if (v.y - center.y < 0.0) 0 else 1

      nodes(x)(y) = nodes(x)(y).insert(v)
      s = s + 1
      this
    } else throw new IllegalArgumentException(s"$v is not inside node: $this")
  }


  override def radialQuery(qv: Vec[2], radiusSquared: Double): NArray[Vec[2]] = {
    val matches: NArrayBuilder[Vec[2]] = NArrayBuilder[Vec[2]]()

    var xi = 0
    while (xi < 2) {
      var yi = 0
      while (yi < 2) {
        if (nodes(xi)(yi).intersects(qv, radiusSquared)) {
          matches.addAll(nodes(xi)(yi).radialQuery(qv, radiusSquared))
        }
        yi = yi + 1
      }
      xi = xi + 1
    }

    matches.result
  }

  override def toString: String = s"MetaPRQuadrant(center = ${center.show}, extent = $extent, maxNodeCapacity = $maxNodeCapacity, size = $size)"
}

class LeafPRQuadrant(override val center: Vec[2], override val extent: Double, maxNodeCapacity: Int = 64) extends PRQuadrant {
  val points: NArrayBuilder[Vec[2]] = NArrayBuilder[Vec[2]]()

  override inline def size: Int = points.size

  def insert(v: Vec[2]): PRQuadrant = {
    if (this.encompasses(v)) {
      if (size < maxNodeCapacity) {
        points.addOne(v)
        this
      } else {
        split().insert(v)
      }
    } else throw new IllegalArgumentException(s"$v is not inside node: $this")
  }

  private def split(): PRQuadrant = {
    val replacementNode = new MetaPRQuadrant(center, extent, maxNodeCapacity)

    var i: Int = 0
    while (i < points.size) {
      val p = points(i)
      replacementNode.insert(p)
      i += 1
    }

    replacementNode
  }

  override def radialQuery(qv: Vec[2], radiusSquared: Double): NArray[Vec[2]] = {
    val matches: NArrayBuilder[Vec[2]] = NArrayBuilder[Vec[2]]()

    var pi: Int = 0
    while (pi < points.size) {
      val pC = points(pi)
      if (pC.euclideanDistanceSquaredTo(qv) <= radiusSquared) matches.addOne(pC)
      pi += 1
    }

    matches.result
  }

  override def toString: String = s"LeafPRQuadrant(center = ${center.show}, extent = $extent, maxNodeCapacity = $maxNodeCapacity, size = $size)"
}
