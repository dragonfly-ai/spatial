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
import vectorf.*

import scala.collection.mutable
import scala.reflect.ClassTag

class PROctreeFMap[T:ClassTag](extent: Float, center:VecF[3] = VecF[3](0.0, 0.0, 0.0), maxNodeCapacity:Int = 64){

  private var root: PROctantFMap[T] = new LeafPROctantFMap(center, extent, maxNodeCapacity)
  val keys: NArrayBuilder[VecF[3]] = NArrayBuilder[VecF[3]]()
  val values: NArrayBuilder[T] = NArrayBuilder[T]()

  def insert(v: VecF[3], value:T): Boolean = synchronized {
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

  inline def encompasses(qv: VecF[3]):Boolean = root.encompasses(qv)

//  def nearestNeighbor(qv: VecF[3]): (VecF[3], T) = {
//    val (nnv, nnid) = root.nearestNeighbor(qv)
//    (nnv, values(nnid))
//  }

  def nearestNeighbor(qv: VecF[3]): (VecF[3], T) = {
    if (this.size < 1) throw IllegalArgumentException(
      s"knn can't find a nearest neighbor in PROctreeMap of size: ${this.size}."
    )

    var nn: VecF[3] = null.asInstanceOf[VecF[3]]
    var nni: Int = -1
    var minDistSquared: Double = Double.MaxValue

    def search(node: PROctantFMap[T]): Unit = node match {
      case lo: LeafPROctantFMap[T] =>
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
      case mo: MetaPROctantFMap[T] =>
        // Check distance to node boundary
        val closestDistSqrd = mo.minDistanceSquaredTo(qv)
        if (nn == null.asInstanceOf[VecF[3]] || closestDistSqrd < minDistSquared) mo.foreachNode(search)
    }

    search(root)
    (nn, values(nni))
  }

  // K-Nearest Neighbor Search
  def knn(qv: VecF[3], k: Int): NArray[(VecF[3], T)] = {
    if (this.size < k) throw IllegalArgumentException(
      s"knn can't find $k nearest neighbors in PROctreeMap of size: ${this.size}."
    )

    case class Candidate(point: (VecF[3], Int), distanceSquared: Double)
    given ord: Ordering[Candidate] = Ordering.by(_.distanceSquared) // Min-heap (closest first)
    val pq = mutable.PriorityQueue.empty[Candidate](ord) // Max-heap (farthest first)

    def search(node: PROctantFMap[T]): Unit = node match {
      case lo: LeafPROctantFMap[T] =>
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
      case mo: MetaPROctantFMap[T] =>
        // Check distance to node boundary
        val closestDist = mo.minDistanceSquaredTo(qv)
        if (pq.size < k || closestDist < pq.head.distanceSquared) mo.foreachNode(search)
    }

    search(root)

    NArray.tabulate[(VecF[3], T)](pq.size)(
      _ => {
        val (mv, mid) = pq.dequeue().point
        (mv, values(mid))
      }
    )
  }

  def radialQuery(pq: VecF[3], radius: Float): NArray[(VecF[3], T)] = {
    val matches = root.radialQuery(pq, squareInPlace(radius))
    NArray.tabulate[(VecF[3], T)](matches.length)(
      (i:Int) => {
        val (mv, mid) = matches(i)
        (mv, values(mid))
      }
    )
  }

  def size: Int = root.size

  def bounds:VectorFBounds[3] = root.bounds

}

trait PROctantFMap[T] extends OctantF {

  def insert(v: VecF[3], id:Int): PROctantFMap[T]

  def radialQuery(qv: VecF[3], radiusSquared: Float): NArray[(VecF[3], Int)]

}

class MetaPROctantFMap[T](override val center:VecF[3], override val extent: Float, maxNodeCapacity:Int = 64) extends PROctantFMap[T] {

  var s:Int = 0

  // Represent Octants in an array
  val nodes: NArray[NArray[NArray[PROctantFMap[T]]]] = {
    val childInfNorm:Float = infNorm / 2f
    val childExtent:Float = infNorm

    NArray[NArray[NArray[PROctantFMap[T]]]](
      NArray[NArray[PROctantFMap[T]]]( // - X
        NArray[PROctantFMap[T]]( // - Y
          LeafPROctantFMap[T](VecF[3](center.x - childInfNorm, center.y - childInfNorm, center.z - childInfNorm), childExtent, maxNodeCapacity), // -X-Y-Z -> 0
          LeafPROctantFMap[T](VecF[3](center.x - childInfNorm, center.y - childInfNorm, center.z + childInfNorm), childExtent, maxNodeCapacity) // -X-Y+Z -> 1
        ),
        NArray[PROctantFMap[T]]( // + Y
          LeafPROctantFMap[T](VecF[3](center.x - childInfNorm, center.y + childInfNorm, center.z - childInfNorm), childExtent, maxNodeCapacity), // -X+Y-Z -> 0
          LeafPROctantFMap[T](VecF[3](center.x - childInfNorm, center.y + childInfNorm, center.z + childInfNorm), childExtent, maxNodeCapacity) // -X+Y+Z -> 1
        )
      ),
      NArray[NArray[PROctantFMap[T]]]( // + X
        NArray[PROctantFMap[T]]( // - Y
          LeafPROctantFMap[T](VecF[3](center.x + childInfNorm, center.y - childInfNorm, center.z - childInfNorm), childExtent, maxNodeCapacity), // +X-Y-Z -> 0
          LeafPROctantFMap[T](VecF[3](center.x + childInfNorm, center.y - childInfNorm, center.z + childInfNorm), childExtent, maxNodeCapacity) // +X-Y+Z -> 1
        ),
        NArray[PROctantFMap[T]]( // + Y
          LeafPROctantFMap[T](VecF[3](center.x + childInfNorm, center.y + childInfNorm, center.z - childInfNorm), childExtent, maxNodeCapacity), // +X+Y-Z -> 0
          LeafPROctantFMap[T](VecF[3](center.x + childInfNorm, center.y + childInfNorm, center.z + childInfNorm), childExtent, maxNodeCapacity) // +X+Y+Z -> 1
        )
      )
    )
  }

  def foreachNode(f:PROctantFMap[T] => Any): Unit = {
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

  override def insert(v: VecF[3], id:Int): PROctantFMap[T] = {
    if (this.encompasses(v)) {
      val x = if (v.x - center.x < 0.0) 0 else 1
      val y = if (v.y - center.y < 0.0) 0 else 1
      val z = if (v.z - center.z < 0.0) 0 else 1

      nodes(x)(y)(z) = nodes(x)(y)(z).insert(v, id)
      s = s + 1
      this
    } else throw new IllegalArgumentException(s"$v is not inside node: $this")
  }

  override def radialQuery(qv: VecF[3], radiusSquared: Float): NArray[(VecF[3], Int)] = {
    val matches: NArrayBuilder[(VecF[3], Int)] = NArrayBuilder[(VecF[3], Int)]()

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

  override def toString: String = s"MetaPROctantFMap(center = ${center.show}, extent = $extent, maxNodeCapacity = $maxNodeCapacity, size = $size)"

}

class LeafPROctantFMap[T](override val center:VecF[3], override val extent: Float, maxNodeCapacity:Int = 64) extends PROctantFMap[T] {
  val points: NArrayBuilder[VecF[3]] = NArrayBuilder[VecF[3]]()
  val ids: NArrayBuilder[Int] = NArrayBuilder[Int]()

  override inline def size:Int = points.size

  def insert(v: VecF[3], id:Int): PROctantFMap[T] = {
    // Verify containment?
    if (this.encompasses(v)) {
      if (size < maxNodeCapacity) {
        points.addOne(v)
        ids.addOne(id)
        this
      } else {
        //println(s"$this.split()")
        split().insert(v, id)
      }
    } else throw new IllegalArgumentException(s"$v is not inside node: $this")
  }

  private def split(): PROctantFMap[T] = {
    val replacementNode = new MetaPROctantFMap[T](center, extent, maxNodeCapacity)
    //println(s"LeafPROctantFMap.split(size = $size, maxNodeCapacity = $maxNodeCapacity) -> $replacementNode")

    var i:Int = 0
    while (i < points.size) {
      val p = points(i)
      val id = ids(i)
      replacementNode.insert(p, id)
      i += 1
    }

    replacementNode
  }

  override def radialQuery(qv: VecF[3], radiusSquared: Float): NArray[(VecF[3], Int)] = {
    val matches: NArrayBuilder[(VecF[3], Int)] = NArrayBuilder[(VecF[3], Int)]()

    var pi: Int = 0
    while (pi < points.size) {
      val pC = points(pi)
      if (pC.euclideanDistanceSquaredTo(qv) <= radiusSquared) matches.addOne((pC, ids(pi)))
      pi += 1
    }

    matches.result
  }

  override def toString: String = s"LeafPROctantFMap(center = ${center.show}, extent = $extent, maxNodeCapacity = $maxNodeCapacity, size = $size)"

}