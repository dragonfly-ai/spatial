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

class PRQuadTreeFMap[T:ClassTag](extent: Float, center: VecF[2] = VecF[2](0.0, 0.0), maxNodeCapacity: Int = 64) {

  private var root: PRQuadrantFMap[T] = new LeafPRQuadrantFMap[T](center, extent, maxNodeCapacity)
  val keys: NArrayBuilder[VecF[2]] = NArrayBuilder[VecF[2]]()
  val values: NArrayBuilder[T] = NArrayBuilder[T]()

  def insert(v: VecF[2], value:T): Boolean = synchronized {
    if (encompasses(v)) {
      val id = root.size
      root = root.insert(v, id)
      if (id + 1 == root.size) {
        keys.addOne(v)
        values.addOne(value)
        true
      } else false
    } else false
  }

  inline def encompasses(qv: VecF[2]): Boolean = root.encompasses(qv)

  def nearestNeighbor(qv: VecF[2]): (VecF[2], T) = {
    if (this.size < 1) throw IllegalArgumentException(
      s"knn can't find a nearest neighbor in PROctreeMap of size: ${this.size}."
    )

    var nn: VecF[2] = null.asInstanceOf[VecF[2]]
    var nni: Int = -1
    var minDistSquared: Double = Double.MaxValue

    def search(node: PRQuadrantFMap[T]): Unit = node match {
      case lo: LeafPRQuadrantFMap[T] =>
        var pi = 0
        while (pi < lo.points.size) {
          val p = lo.points(pi)
          val pid = lo.ids(pi)
          val dstSq = qv.euclideanDistanceSquaredTo(p) //(v - qv).magnitude
          //if (pq.size < k) pq.enqueue(Candidate((p, pid), dstSq))
          if (dstSq < minDistSquared) {
            nn = p
            nni = pid
            minDistSquared = dstSq
          }
          pi = pi + 1
        }
      case mo: MetaPRQuadrantFMap[T] =>
        // Check distance to node boundary
        val closestDistSqrd = mo.minDistanceSquaredTo(qv)
        if (nn == null.asInstanceOf[VecF[2]] || closestDistSqrd < minDistSquared) mo.foreachNode(search)
    }

    search(root)
    (nn, values(nni))
  }

  // K-Nearest Neighbor Search
  def knn(qv: VecF[2], k: Int): NArray[(VecF[2], T)] = {
    if (this.size < k) throw IllegalArgumentException(
      s"knn can't find $k nearest neighbors in PROctreeMap of size: ${this.size}."
    )

    case class Candidate(point: (VecF[2], Int), distanceSquared: Double)
    given ord: Ordering[Candidate] = Ordering.by(_.distanceSquared) // Min-heap (closest first)
    val pq = mutable.PriorityQueue.empty[Candidate](ord) // Max-heap (farthest first)

    def search(node: PRQuadrantFMap[T]): Unit = node match {
      case lo: LeafPRQuadrantFMap[T] =>
        var pi = 0
        while (pi < lo.points.size) {
          val p = lo.points(pi)
          val pid = lo.ids(pi)
          val dstSq = qv.euclideanDistanceSquaredTo(p) //(v - qv).magnitude
          if (pq.size < k) pq.enqueue(Candidate((p, pid), dstSq))
          else if (dstSq < pq.head.distanceSquared) {
            pq.dequeue()
            pq.enqueue(Candidate((p, pid), dstSq))
          }
          pi = pi + 1
        }
      case mo: MetaPRQuadrantFMap[T] =>
        // Check distance to node boundary
        val closestDist = mo.minDistanceSquaredTo(qv)
        if (pq.size < k || closestDist < pq.head.distanceSquared) mo.foreachNode(search)
    }

    search(root)

    NArray.tabulate[(VecF[2], T)](pq.size)(
      _ => {
        val (mv, mid) = pq.dequeue().point
        (mv, values(mid))
      }
    )
  }

  def radialQuery(pq: VecF[2], radius: Float): NArray[(VecF[2], T)] = {
    val matches = root.radialQuery(pq, squareInPlace(radius))
    NArray.tabulate[(VecF[2], T)](matches.length)(
      (i:Int) => {
        val (mv, mid) = matches(i)
        (mv, values(mid))
      }
    )
  }
  def size: Int = root.size

  def bounds: VectorFBounds[2] = root.bounds
}

trait PRQuadrantFMap[T] extends QuadrantF {

  def insert(v: VecF[2], id: Int): PRQuadrantFMap[T]

  def radialQuery(qv: VecF[2], radiusSquared: Float): NArray[(VecF[2], Int)]

}

class MetaPRQuadrantFMap[T](override val center: VecF[2], override val extent: Float, maxNodeCapacity: Int = 64) extends PRQuadrantFMap[T] {

  var s: Int = 0

  // Represent Quadrants in a 2x2 array
  val nodes: NArray[NArray[PRQuadrantFMap[T]]] = {
    val childInfNorm: Float = infNorm / 2f
    val childExtent: Float = infNorm

    NArray[NArray[PRQuadrantFMap[T]]](
      NArray[PRQuadrantFMap[T]]( // -X
        LeafPRQuadrantFMap[T](VecF[2](center.x - childInfNorm, center.y - childInfNorm), childExtent, maxNodeCapacity), // SW: -X-Y
        LeafPRQuadrantFMap[T](VecF[2](center.x - childInfNorm, center.y + childInfNorm), childExtent, maxNodeCapacity)  // NW: -X+Y
      ),
      NArray[PRQuadrantFMap[T]]( // +X
        LeafPRQuadrantFMap[T](VecF[2](center.x + childInfNorm, center.y - childInfNorm), childExtent, maxNodeCapacity), // SE: +X-Y
        LeafPRQuadrantFMap[T](VecF[2](center.x + childInfNorm, center.y + childInfNorm), childExtent, maxNodeCapacity)  // NE: +X+Y
      )
    )
  }

  def foreachNode(f: PRQuadrantFMap[T] => Any): Unit = {
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

  override def insert(v: VecF[2], id:Int): PRQuadrantFMap[T] = {
    if (this.encompasses(v)) {
      val x = if (v.x - center.x < 0.0) 0 else 1
      val y = if (v.y - center.y < 0.0) 0 else 1

      nodes(x)(y) = nodes(x)(y).insert(v, id)
      s = s + 1
      this
    } else throw new IllegalArgumentException(s"$v is not inside node: $this")
  }

  override def radialQuery(qv: VecF[2], radiusSquared: Float): NArray[(VecF[2], Int)] = {
    val matches: NArrayBuilder[(VecF[2], Int)] = NArrayBuilder[(VecF[2], Int)]()

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

class LeafPRQuadrantFMap[T](override val center: VecF[2], override val extent: Float, maxNodeCapacity: Int = 64) extends PRQuadrantFMap[T] {
  val points: NArrayBuilder[VecF[2]] = NArrayBuilder[VecF[2]]()
  val ids: NArrayBuilder[Int] = NArrayBuilder[Int]()

  override inline def size: Int = points.size

  def insert(v: VecF[2], id:Int): PRQuadrantFMap[T] = {
    if (this.encompasses(v)) {
      if (size < maxNodeCapacity) {
        points.addOne(v)
        ids.addOne(id)
        this
      } else {
        split().insert(v, id)
      }
    } else throw new IllegalArgumentException(s"$v is not inside node: $this")
  }

  private def split(): PRQuadrantFMap[T] = {
    val replacementNode = new MetaPRQuadrantFMap[T](center, extent, maxNodeCapacity)

    var i:Int = 0
    while (i < points.size) {
      val p = points(i)
      val id = ids(i)
      replacementNode.insert(p, id)
      i += 1
    }

    replacementNode
  }

  override def radialQuery(qv: VecF[2], radiusSquared: Float): NArray[(VecF[2], Int)] = {
    val matches: NArrayBuilder[(VecF[2], Int)] = NArrayBuilder[(VecF[2], Int)]()

    var pi: Int = 0
    while (pi < points.size) {
      val pC = points(pi)
      if (pC.euclideanDistanceSquaredTo(qv) <= radiusSquared) matches.addOne((pC, ids(pi)))
      pi += 1
    }

    matches.result
  }

  override def toString: String = s"LeafPRQuadrant(center = ${center.show}, extent = $extent, maxNodeCapacity = $maxNodeCapacity, size = $size)"
}
