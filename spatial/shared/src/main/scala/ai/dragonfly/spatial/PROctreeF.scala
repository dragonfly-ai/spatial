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
import scala.reflect.ClassTag

/*
This implementation of octree lacks:
  - spatially ordered iterator?
 */

class PROctreeF(extent: Float, center:VecF[3] = VecF[3](0f, 0f, 0f), maxNodeCapacity:Int = 64) {

  private var root: PROctantF = new LeafPROctantF(center, extent, maxNodeCapacity)

  def insert(v: VecF[3]): Boolean = synchronized {
    if (encompasses(v)) {
      val s = root.size
      root = root.insert(v)
      s + 1 == root.size
    } else false
  }

  inline def encompasses(qv: VecF[3]):Boolean = root.encompasses(qv)

  def nearestNeighbor(qv: VecF[3]): VecF[3] = {
    if (this.size < 1) throw IllegalArgumentException(
      s"knn can't find a nearest neighbor in PROctree of size: ${this.size}."
    )

    var nn: VecF[3] = null.asInstanceOf[VecF[3]]
    var minDistSquared: Double = Double.MaxValue

    def search(node: PROctantF): Unit = node match {
      case lo: LeafPROctantF =>
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
      case mo: MetaPROctantF =>
        // Check distance to node boundary
        val closestDistSqrd = mo.minDistanceSquaredTo(qv)
        if (nn == null.asInstanceOf[VecF[3]] || closestDistSqrd < minDistSquared) mo.foreachNode(search)
    }

    search(root)
    nn
  }

  // K-Nearest Neighbor Search
  def knn(qv: VecF[3], k: Int): NArray[VecF[3]] = {
    if (this.size < k) throw IllegalArgumentException(
      s"knn can't find $k nearest neighbors in PROctreeMap of size: ${this.size}."
    )

    case class Candidate(point: VecF[3], distanceSquared: Double)
    given ord: Ordering[Candidate] = Ordering.by(_.distanceSquared) // Min-heap (closest first)
    val pq = mutable.PriorityQueue.empty[Candidate](ord) // Max-heap (farthest first)

    def search(node: PROctantF): Unit = node match {
      case lo: LeafPROctantF =>
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
      case mo: MetaPROctantF =>
        // Check distance to node boundary
        val closestDistSqrd = mo.minDistanceSquaredTo(qv)
        if (pq.size < k || closestDistSqrd < pq.head.distanceSquared) mo.foreachNode(search)
    }

    search(root)

    NArray.tabulate[VecF[3]](pq.size)(_ => pq.dequeue().point)
  }

  def radialQuery(pq: VecF[3], radius: Float): NArray[VecF[3]] = root.radialQuery(pq, squareInPlace(radius))

  def size: Int = root.size

  def bounds:VectorFBounds[3] = root.bounds

}


trait PROctantF extends OctantF {

  def insert(v: VecF[3]): PROctantF

  def radialQuery(qv: VecF[3], radiusSquared: Float): NArray[VecF[3]]

}


class MetaPROctantF(override val center:VecF[3], override val extent: Float, maxNodeCapacity:Int = 64) extends PROctantF {

  var s:Int = 0

  // Represent Octants in an array
  val nodes: NArray[NArray[NArray[PROctantF]]] = {
    val childInfNorm:Float = infNorm / 2f
    val childExtent:Float = infNorm

    NArray[NArray[NArray[PROctantF]]](
      NArray[NArray[PROctantF]]( // - X
        NArray[PROctantF]( // - Y
          LeafPROctantF(VecF[3](center.x - childInfNorm, center.y - childInfNorm, center.z - childInfNorm), childExtent, maxNodeCapacity), // -X-Y-Z -> 0
          LeafPROctantF(VecF[3](center.x - childInfNorm, center.y - childInfNorm, center.z + childInfNorm), childExtent, maxNodeCapacity) // -X-Y+Z -> 1
        ),
        NArray[PROctantF]( // + Y
          LeafPROctantF(VecF[3](center.x - childInfNorm, center.y + childInfNorm, center.z - childInfNorm), childExtent, maxNodeCapacity), // -X+Y-Z -> 0
          LeafPROctantF(VecF[3](center.x - childInfNorm, center.y + childInfNorm, center.z + childInfNorm), childExtent, maxNodeCapacity) // -X+Y+Z -> 1
        )
      ),
      NArray[NArray[PROctantF]]( // + X
        NArray[PROctantF]( // - Y
          LeafPROctantF(VecF[3](center.x + childInfNorm, center.y - childInfNorm, center.z - childInfNorm), childExtent, maxNodeCapacity), // +X-Y-Z -> 0
          LeafPROctantF(VecF[3](center.x + childInfNorm, center.y - childInfNorm, center.z + childInfNorm), childExtent, maxNodeCapacity) // +X-Y+Z -> 1
        ),
        NArray[PROctantF]( // + Y
          LeafPROctantF(VecF[3](center.x + childInfNorm, center.y + childInfNorm, center.z - childInfNorm), childExtent, maxNodeCapacity), // +X+Y-Z -> 0
          LeafPROctantF(VecF[3](center.x + childInfNorm, center.y + childInfNorm, center.z + childInfNorm), childExtent, maxNodeCapacity) // +X+Y+Z -> 1
        )
      )
    )
  }

  def foreachNode(f:PROctantF => Any): Unit = {
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

  override def insert(v: VecF[3]): PROctantF = {
    if (this.encompasses(v)) {
      val x = if (v.x - center.x < 0.0) 0 else 1
      val y = if (v.y - center.y < 0.0) 0 else 1
      val z = if (v.z - center.z < 0.0) 0 else 1

      nodes(x)(y)(z) = nodes(x)(y)(z).insert(v)
      s = s + 1
      this
    } else throw new IllegalArgumentException(s"$v is not inside node: $this")
  }

  override def radialQuery(qv: VecF[3], radiusSquared: Float): NArray[VecF[3]] = {

    val matches: NArrayBuilder[VecF[3]] = NArrayBuilder[VecF[3]]()

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

  override def toString: String = s"MetaPROctantF(center = ${center.show}, extent = $extent, maxNodeCapacity = $maxNodeCapacity, size = $size)"

}

class LeafPROctantF(override val center:VecF[3], override val extent: Float, maxNodeCapacity:Int = 64) extends PROctantF {
  //val points: NArray[VecF[3]] = NArray.fill[VecF[3]](maxCapacitymaxNodeCapacity)(null.asInstanceOf[VecF[3]])
  val points: NArrayBuilder[VecF[3]] = NArrayBuilder[VecF[3]]()

  override inline def size:Int = points.size

  def insert(v: VecF[3]): PROctantF = {
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

  private def split(): PROctantF = {
    val replacementNode = new MetaPROctantF(center, extent, maxNodeCapacity)
    //println(s"LeafPROctantF.split(size = $size, maxNodeCapacity = $maxNodeCapacity) -> $replacementNode")

    var i:Int = 0
    while (i < points.size) {
      val p = points(i)
      replacementNode.insert(p)
      i += 1
    }

    replacementNode
  }

  override def radialQuery(qv: VecF[3], radiusSquared: Float): NArray[VecF[3]] = {
    val matches: NArrayBuilder[VecF[3]] = NArrayBuilder[VecF[3]]()

    var pi: Int = 0
    while (pi < points.size) {
      val pC = points(pi)
      if (pC.euclideanDistanceSquaredTo(qv) <= radiusSquared) matches.addOne(pC)
      pi += 1
    }

    matches.result
  }

  override def toString: String = s"LeafPROctantF(center = ${center.show}, extent = $extent, maxNodeCapacity = $maxNodeCapacity, size = $size)"

}