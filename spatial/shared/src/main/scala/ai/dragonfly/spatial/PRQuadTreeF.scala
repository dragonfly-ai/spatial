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
import slash.vectorf.*

import scala.collection.mutable

class PRQuadTreeF(extent: Float, center: VecF[2] = VecF[2](0.0, 0.0), maxNodeCapacity: Int = 64) {

  private var root: PRQuadrantF = new LeafPRQuadrantF(center, extent, maxNodeCapacity)

  def insert(v: VecF[2]): Boolean = synchronized {
    if (encompasses(v)) {
      val s = root.size
      root = root.insert(v)
      s + 1 == root.size
    } else false
  }

  inline def encompasses(qv: VecF[2]): Boolean = root.encompasses(qv)

  def nearestNeighbor(qv: VecF[2]): VecF[2] = {
    if (this.size < 1) throw IllegalArgumentException(
      s"knn can't find a nearest neighbor in PROctree of size: ${this.size}."
    )

    var nn: VecF[2] = null.asInstanceOf[VecF[2]]
    var minDistSquared: Double = Double.MaxValue

    def search(node: PRQuadrantF): Unit = node match {
      case lo: LeafPRQuadrantF =>
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
      case mo: MetaPRQuadrantF =>
        // Check distance to node boundary
        val closestDistSqrd = mo.minDistanceSquaredTo(qv)
        if (nn == null.asInstanceOf[VecF[2]] || closestDistSqrd < minDistSquared) mo.foreachNode(search)
    }

    search(root)
    nn
  }

  // K-Nearest Neighbor Search
  def knn(qv: VecF[2], k: Int): NArray[VecF[2]] = {
    if (this.size < k) throw IllegalArgumentException(
      s"knn can't find $k nearest neighbors in PRQuadFTree of size: ${this.size}."
    )

    case class Candidate(point: VecF[2], distanceSquared: Double)
    given ord: Ordering[Candidate] = Ordering.by(_.distanceSquared) // Min-heap (closest first)
    val pq = mutable.PriorityQueue.empty[Candidate](ord)

    def search(node: PRQuadrantF): Unit = node match {
      case lq: LeafPRQuadrantF =>
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
      case mq: MetaPRQuadrantF =>
        val closestDist = mq.minDistanceSquaredTo(qv)
        if (pq.size < k || closestDist < pq.head.distanceSquared) mq.foreachNode(search)
    }

    search(root)

    NArray.tabulate[VecF[2]](pq.size)(_ => pq.dequeue().point)
  }

  def radialQuery(pq: VecF[2], radius: Float): NArray[VecF[2]] = root.radialQuery(pq, squareInPlace(radius))

  def size: Int = root.size

  def bounds: VectorFBounds[2] = root.bounds
}

trait PRQuadrantF extends QuadrantF {

  def insert(v: VecF[2]): PRQuadrantF

  def radialQuery(qv: VecF[2], radiusSquared: Float): NArray[VecF[2]]
}

class MetaPRQuadrantF(override val center: VecF[2], override val extent: Float, maxNodeCapacity: Int = 64) extends PRQuadrantF {

  var s: Int = 0

  // Represent Quadrants in a 2x2 array
  val nodes: NArray[NArray[PRQuadrantF]] = {
    val childInfNorm: Float = infNorm / 2f
    val childExtent: Float = infNorm

    NArray[NArray[PRQuadrantF]](
      NArray[PRQuadrantF]( // -X
        LeafPRQuadrantF(VecF[2](center.x - childInfNorm, center.y - childInfNorm), childExtent, maxNodeCapacity), // SW: -X-Y
        LeafPRQuadrantF(VecF[2](center.x - childInfNorm, center.y + childInfNorm), childExtent, maxNodeCapacity)  // NW: -X+Y
      ),
      NArray[PRQuadrantF]( // +X
        LeafPRQuadrantF(VecF[2](center.x + childInfNorm, center.y - childInfNorm), childExtent, maxNodeCapacity), // SE: +X-Y
        LeafPRQuadrantF(VecF[2](center.x + childInfNorm, center.y + childInfNorm), childExtent, maxNodeCapacity)  // NE: +X+Y
      )
    )
  }

  def foreachNode(f: PRQuadrantF => Any): Unit = {
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

  override def insert(v: VecF[2]): PRQuadrantF = {
    if (this.encompasses(v)) {
      val x = if (v.x - center.x < 0.0) 0 else 1
      val y = if (v.y - center.y < 0.0) 0 else 1

      nodes(x)(y) = nodes(x)(y).insert(v)
      s = s + 1
      this
    } else throw new IllegalArgumentException(s"$v is not inside node: $this")
  }


  override def radialQuery(qv: VecF[2], radiusSquared: Float): NArray[VecF[2]] = {
    val matches: NArrayBuilder[VecF[2]] = NArrayBuilder[VecF[2]]()

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

  override def toString: String = s"MetaPRQuadrantF(center = ${center.show}, extent = $extent, maxNodeCapacity = $maxNodeCapacity, size = $size)"
}

class LeafPRQuadrantF(override val center: VecF[2], override val extent: Float, maxNodeCapacity: Int = 64) extends PRQuadrantF {
  val points: NArrayBuilder[VecF[2]] = NArrayBuilder[VecF[2]]()

  override inline def size: Int = points.size

  def insert(v: VecF[2]): PRQuadrantF = {
    if (this.encompasses(v)) {
      if (size < maxNodeCapacity) {
        points.addOne(v)
        this
      } else {
        split().insert(v)
      }
    } else throw new IllegalArgumentException(s"$v is not inside node: $this")
  }

  private def split(): PRQuadrantF = {
    val replacementNode = new MetaPRQuadrantF(center, extent, maxNodeCapacity)

    var i: Int = 0
    while (i < points.size) {
      val p = points(i)
      replacementNode.insert(p)
      i += 1
    }

    replacementNode
  }

  override def radialQuery(qv: VecF[2], radiusSquared: Float): NArray[VecF[2]] = {
    val matches: NArrayBuilder[VecF[2]] = NArrayBuilder[VecF[2]]()

    var pi: Int = 0
    while (pi < points.size) {
      val pC = points(pi)
      if (pC.euclideanDistanceSquaredTo(qv) <= radiusSquared) matches.addOne(pC)
      pi += 1
    }

    matches.result
  }

  override def toString: String = s"LeafPRQuadrantF(center = ${center.show}, extent = $extent, maxNodeCapacity = $maxNodeCapacity, size = $size)"
}
