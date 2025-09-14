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
import scala.reflect.ClassTag

/*
This implementation of octree lacks:
  - spatially ordered iterator?
 */

class PROctree(extent: Double, center:Vec[3] = Vec[3](0.0, 0.0, 0.0), maxNodeCapacity:Int = 64) {

  private var root: PROctant = new LeafPROctant(center, extent, maxNodeCapacity)

  def insert(v: Vec[3]): Boolean = synchronized {
    if (encompasses(v)) {
      val s = root.size
      root = root.insert(v)
      s + 1 == root.size
    } else false
  }

  inline def encompasses(qv: Vec[3]):Boolean = root.encompasses(qv)

  def nearestNeighbor(qv: Vec[3]): Vec[3] = {
    if (this.size < 1) throw IllegalArgumentException(
      s"knn can't find a nearest neighbor in PROctree of size: ${this.size}."
    )

    var nn: Vec[3] = null.asInstanceOf[Vec[3]]
    var minDistSquared: Double = Double.MaxValue

    def search(node: PROctant): Unit = node match {
      case lo: LeafPROctant =>
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
      case mo: MetaPROctant =>
        // Check distance to node boundary
        val closestDistSqrd = mo.minDistanceSquaredTo(qv)
        if (nn == null.asInstanceOf[Vec[3]] || closestDistSqrd < minDistSquared) mo.foreachNode(search)
    }

    search(root)
    nn
  }

  // K-Nearest Neighbor Search
  def knn(qv: Vec[3], k: Int): NArray[Vec[3]] = {
    if (this.size < k) throw IllegalArgumentException(
      s"knn can't find $k nearest neighbors in PROctreeMap of size: ${this.size}."
    )

    case class Candidate(point: Vec[3], distanceSquared: Double)
    given ord: Ordering[Candidate] = Ordering.by(_.distanceSquared) // Min-heap (closest first)
    val pq = mutable.PriorityQueue.empty[Candidate](ord) // Max-heap (farthest first)

    def search(node: PROctant): Unit = node match {
      case lo: LeafPROctant =>
        var pi = 0
        while (pi < lo.points.size) {
          val p = lo.points(pi)
          val dstSq = qv.euclideanDistanceSquaredTo(p) //(v - qv).magnitude
          if (pq.size < k) pq.enqueue(Candidate(p, dstSq))
          else if (dstSq < pq.head.distanceSquared) {
            pq.dequeue()
            pq.enqueue(Candidate(p, dstSq))
          }
          pi = pi + 1
        }
      case mo: MetaPROctant =>
        // Check distance to node boundary
        val closestDistSqrd = mo.minDistanceSquaredTo(qv)
        if (pq.size < k || closestDistSqrd < pq.head.distanceSquared) mo.foreachNode(search)
    }

    search(root)

    NArray.tabulate[Vec[3]](pq.size)(_ => pq.dequeue().point)
  }

  def radialQuery(pq: Vec[3], radius: Double): NArray[Vec[3]] = root.radialQuery(pq, squareInPlace(radius))

  def size: Int = root.size

  def bounds:VectorBounds[3] = root.bounds

}


trait PROctant extends Octant {

  def insert(v: Vec[3]): PROctant

  def radialQuery(qv: Vec[3], radiusSquared: Double): NArray[Vec[3]]

}


class MetaPROctant(override val center:Vec[3], override val extent: Double, maxNodeCapacity:Int = 64) extends PROctant {

  var s:Int = 0

  // Represent Octants in an array
  val nodes: NArray[NArray[NArray[PROctant]]] = {
    val childInfNorm:Double = infNorm / 2.0
    val childExtent:Double = infNorm

    NArray[NArray[NArray[PROctant]]](
      NArray[NArray[PROctant]]( // - X
        NArray[PROctant]( // - Y
          LeafPROctant(Vec[3](center.x - childInfNorm, center.y - childInfNorm, center.z - childInfNorm), childExtent, maxNodeCapacity), // -X-Y-Z -> 0
          LeafPROctant(Vec[3](center.x - childInfNorm, center.y - childInfNorm, center.z + childInfNorm), childExtent, maxNodeCapacity) // -X-Y+Z -> 1
        ),
        NArray[PROctant]( // + Y
          LeafPROctant(Vec[3](center.x - childInfNorm, center.y + childInfNorm, center.z - childInfNorm), childExtent, maxNodeCapacity), // -X+Y-Z -> 0
          LeafPROctant(Vec[3](center.x - childInfNorm, center.y + childInfNorm, center.z + childInfNorm), childExtent, maxNodeCapacity) // -X+Y+Z -> 1
        )
      ),
      NArray[NArray[PROctant]]( // + X
        NArray[PROctant]( // - Y
          LeafPROctant(Vec[3](center.x + childInfNorm, center.y - childInfNorm, center.z - childInfNorm), childExtent, maxNodeCapacity), // +X-Y-Z -> 0
          LeafPROctant(Vec[3](center.x + childInfNorm, center.y - childInfNorm, center.z + childInfNorm), childExtent, maxNodeCapacity) // +X-Y+Z -> 1
        ),
        NArray[PROctant]( // + Y
          LeafPROctant(Vec[3](center.x + childInfNorm, center.y + childInfNorm, center.z - childInfNorm), childExtent, maxNodeCapacity), // +X+Y-Z -> 0
          LeafPROctant(Vec[3](center.x + childInfNorm, center.y + childInfNorm, center.z + childInfNorm), childExtent, maxNodeCapacity) // +X+Y+Z -> 1
        )
      )
    )
  }

  def foreachNode(f:PROctant => Any): Unit = {
    var xi = 0
    while (xi < 2) {
      var yi = 0
      while (yi < 2) {
        var zi = 0
        while (zi < 2) {
          f(nodes(xi)(yi)(zi))
          zi = zi + 1
        }
        yi = yi + 1
      }
      xi = xi + 1
    }
  }

  override inline def size:Int = s

  override def insert(v: Vec[3]): PROctant = {
    if (this.encompasses(v)) {
      val x = if (v.x - center.x < 0.0) 0 else 1
      val y = if (v.y - center.y < 0.0) 0 else 1
      val z = if (v.z - center.z < 0.0) 0 else 1

      nodes(x)(y)(z) = nodes(x)(y)(z).insert(v)
      s = s + 1
      this
    } else throw new IllegalArgumentException(s"$v is not inside node: $this")
  }

  override def radialQuery(qv: Vec[3], radiusSquared: Double): NArray[Vec[3]] = {

    val matches: NArrayBuilder[Vec[3]] = NArrayBuilder[Vec[3]]()

    var xi = 0
    while (xi < 2) {
      var yi = 0
      while (yi < 2) {
        var zi = 0
        while (zi < 2) {
          if (nodes(xi)(yi)(zi).intersects(qv, radiusSquared)) {
            matches.addAll(nodes(xi)(yi)(zi).radialQuery(qv, radiusSquared))
          }
          zi = zi + 1
        }
        yi = yi + 1
      }
      xi = xi + 1
    }

    matches.result
  }

  override def toString: String = s"MetaPROctant(center = ${center.show}, extent = $extent, maxNodeCapacity = $maxNodeCapacity, size = $size)"

}

class LeafPROctant(override val center:Vec[3], override val extent: Double, maxNodeCapacity:Int = 64) extends PROctant {
  //val points: NArray[Vec[3]] = NArray.fill[Vec[3]](maxCapacitymaxNodeCapacity)(null.asInstanceOf[Vec[3]])
  val points: NArrayBuilder[Vec[3]] = NArrayBuilder[Vec[3]]()

  override inline def size:Int = points.size

  def insert(v: Vec[3]): PROctant = {
    // Verify containment?
    if (this.encompasses(v)) {
      if (size < maxNodeCapacity) {
        points.addOne(v)
        this
      } else {
        //println(s"$this.split()")
        split().insert(v)
      }
    } else throw new IllegalArgumentException(s"$v is not inside node: $this")
  }

  private def split(): PROctant = {
    val replacementNode = new MetaPROctant(center, extent, maxNodeCapacity)
    //println(s"LeafPROctant.split(size = $size, maxNodeCapacity = $maxNodeCapacity) -> $replacementNode")

    var i:Int = 0
    while (i < points.size) {
      val p = points(i)
      replacementNode.insert(p)
      i += 1
    }

    replacementNode
  }

  override def radialQuery(qv: Vec[3], radiusSquared: Double): NArray[Vec[3]] = {
    val matches: NArrayBuilder[Vec[3]] = NArrayBuilder[Vec[3]]()

    var pi: Int = 0
    while (pi < points.size) {
      val pC = points(pi)
      if (pC.euclideanDistanceSquaredTo(qv) <= radiusSquared) matches.addOne(pC)
      pi += 1
    }

    matches.result
  }

  override def toString: String = s"LeafPROctant(center = ${center.show}, extent = $extent, maxNodeCapacity = $maxNodeCapacity, size = $size)"

}