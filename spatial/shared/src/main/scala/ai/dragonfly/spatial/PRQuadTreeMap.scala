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

class PRQuadTreeMap[T:ClassTag](extent: Double, center: Vec[2] = Vec[2](0.0, 0.0), maxNodeCapacity: Int = 64) {

  private var root: PRQuadrantMap[T] = new LeafPRQuadrantMap[T](center, extent, maxNodeCapacity)
  val keys: NArrayBuilder[Vec[2]] = NArrayBuilder[Vec[2]]()
  val values: NArrayBuilder[T] = NArrayBuilder[T]()

  def insert(v: Vec[2], value:T): Boolean = synchronized {
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

  inline def encompasses(qv: Vec[2]): Boolean = root.encompasses(qv)

  def nearestNeighbor(qv: Vec[2]): (Vec[2], T) = {
    val (nnv, nnid) = root.nearestNeighbor(qv)
    (nnv, values(nnid))
  }

  // K-Nearest Neighbor Search
  def knn(qv: Vec[2], k: Int): NArray[(Vec[2], T)] = {
    if (this.size < k) throw IllegalArgumentException(
      s"knn can't find $k nearest neighbors in PROctreeMap of size: ${this.size}."
    )

    case class Candidate(point: (Vec[2], Int), distanceSquared: Double)
    given ord: Ordering[Candidate] = Ordering.by(_.distanceSquared) // Min-heap (closest first)
    val pq = mutable.PriorityQueue.empty[Candidate](ord) // Max-heap (farthest first)

    def search(node: PRQuadrantMap[T]): Unit = node match {
      case lo: LeafPRQuadrantMap[T] =>
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
      case mo: MetaPRQuadrantMap[T] =>
        // Check distance to node boundary
        val closestDist = mo.minDistanceSquaredTo(qv)
        if (pq.size < k || closestDist < pq.head.distanceSquared) mo.foreachNode(search)
    }

    search(root)

    NArray.tabulate[(Vec[2], T)](pq.size)(
      _ => {
        val (mv, mid) = pq.dequeue().point
        (mv, values(mid))
      }
    )
  }

  def radialQuery(pq: Vec[2], radius: Double): NArray[(Vec[2], T)] = {
    val matches = root.radialQuery(pq, squareInPlace(radius))
    NArray.tabulate[(Vec[2], T)](matches.length)(
      (i:Int) => {
        val (mv, mid) = matches(i)
        (mv, values(mid))
      }
    )
  }
  def size: Int = root.size

  def bounds: VectorBounds[2] = root.bounds
}

trait PRQuadrantMap[T] extends Quadrant {

  def nearestNeighbor(qv: Vec[2]): (Vec[2], Int)

  def insert(v: Vec[2], id: Int): PRQuadrantMap[T]

  def radialQuery(qv: Vec[2], radiusSquared: Double): NArray[(Vec[2], Int)]

}

class MetaPRQuadrantMap[T](override val center: Vec[2], override val extent: Double, maxNodeCapacity: Int = 64) extends PRQuadrantMap[T] {

  var s: Int = 0

  // Represent Quadrants in a 2x2 array
  val nodes: NArray[NArray[PRQuadrantMap[T]]] = {
    val childInfNorm: Double = infNorm / 2.0
    val childExtent: Double = infNorm

    NArray[NArray[PRQuadrantMap[T]]](
      NArray[PRQuadrantMap[T]]( // -X
        LeafPRQuadrantMap[T](Vec[2](center.x - childInfNorm, center.y - childInfNorm), childExtent, maxNodeCapacity), // SW: -X-Y
        LeafPRQuadrantMap[T](Vec[2](center.x - childInfNorm, center.y + childInfNorm), childExtent, maxNodeCapacity)  // NW: -X+Y
      ),
      NArray[PRQuadrantMap[T]]( // +X
        LeafPRQuadrantMap[T](Vec[2](center.x + childInfNorm, center.y - childInfNorm), childExtent, maxNodeCapacity), // SE: +X-Y
        LeafPRQuadrantMap[T](Vec[2](center.x + childInfNorm, center.y + childInfNorm), childExtent, maxNodeCapacity)  // NE: +X+Y
      )
    )
  }

  def foreachNode(f: PRQuadrantMap[T] => Any): Unit = {
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

  override def insert(v: Vec[2], id:Int): PRQuadrantMap[T] = {
    if (this.encompasses(v)) {
      val x = if (v.x - center.x < 0.0) 0 else 1
      val y = if (v.y - center.y < 0.0) 0 else 1

      nodes(x)(y) = nodes(x)(y).insert(v, id)
      s = s + 1
      this
    } else throw new IllegalArgumentException(s"$v is not inside node: $this")
  }

  override def nearestNeighbor(qv: Vec[2]): (Vec[2], Int) = {
    if (this.size < 1) throw new NoSuchElementException("Can't find a nearest neighbor from an empty node.")

    val x = if (qv.x - center.x < 0.0) 0 else 1
    val y = if (qv.y - center.y < 0.0) 0 else 1

    var node = nodes(x)(y)
    var nn = node.nearestNeighbor(qv)
    var nnDistSquared: Double = qv.euclideanDistanceSquaredTo(nn._1)

    var xi = 0
    while (xi < 2) {
      var yi = 0
      while (yi < 2) {
        if (xi != x || yi != y) {
          node = nodes(xi)(yi)
          if (node.size > 0 && node.minDistanceSquaredTo(qv) < nnDistSquared) {
            val candidate = node.nearestNeighbor(qv)
            val cd: Double = qv.euclideanDistanceSquaredTo(candidate._1)
            if (cd < nnDistSquared) {
              nnDistSquared = cd
              nn = candidate
            }
          }
        }
        yi = yi + 1
      }
      xi = xi + 1
    }

    nn
  }

  override def radialQuery(qv: Vec[2], radiusSquared: Double): NArray[(Vec[2], Int)] = {
    val matches: NArrayBuilder[(Vec[2], Int)] = NArrayBuilder[(Vec[2], Int)]()

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

class LeafPRQuadrantMap[T](override val center: Vec[2], override val extent: Double, maxNodeCapacity: Int = 64) extends PRQuadrantMap[T] {
  val points: NArrayBuilder[Vec[2]] = NArrayBuilder[Vec[2]]()
  val ids: NArrayBuilder[Int] = NArrayBuilder[Int]()

  override inline def size: Int = points.size

  def insert(v: Vec[2], id:Int): PRQuadrantMap[T] = {
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

  private def split(): PRQuadrantMap[T] = {
    val replacementNode = new MetaPRQuadrantMap[T](center, extent, maxNodeCapacity)

    var i:Int = 0
    while (i < points.size) {
      val p = points(i)
      val id = ids(i)
      replacementNode.insert(p, id)
      i += 1
    }

    replacementNode
  }

  override def radialQuery(qv: Vec[2], radiusSquared: Double): NArray[(Vec[2], Int)] = {
    val matches: NArrayBuilder[(Vec[2], Int)] = NArrayBuilder[(Vec[2], Int)]()

    var pi: Int = 0
    while (pi < points.size) {
      val pC = points(pi)
      if (pC.euclideanDistanceSquaredTo(qv) <= radiusSquared) matches.addOne((pC, ids(pi)))
      pi += 1
    }

    matches.result
  }

  override def nearestNeighbor(qv: Vec[2]): (Vec[2], Int) = {
    if (points.size < 1) throw new NoSuchElementException("Can't find a nearest neighbor from an empty node.")
    else {
      var nn: (Vec[2], Int) = (points(0), ids(0))
      var minDistSquared = qv.euclideanDistanceSquaredTo(nn._1)

      var i: Int = 1
      while (i < points.size) {
        val candidate = points(i)
        val dist = qv.euclideanDistanceSquaredTo(candidate)
        if (dist < minDistSquared) {
          minDistSquared = dist
          nn = (candidate, ids(i))
        }
        i += 1
      }

      nn
    }
  }

  override def toString: String = s"LeafPRQuadrant(center = ${center.show}, extent = $extent, maxNodeCapacity = $maxNodeCapacity, size = $size)"
}
