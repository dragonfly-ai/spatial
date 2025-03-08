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

import narr.*
import ai.dragonfly.spatial.*
import slash.Random.{defaultRandom as r, *}
import slash.vector.*

import scala.collection.mutable

class LeafOctantTest extends munit.FunSuite {

  val N: Int = 64

  var o: OctantPR = new LeafOctantPR(Vec[3](0.0, 0.0, 0.0), 100.0)
  val all: NArrayBuilder[Vec[3]] = NArrayBuilder[Vec[3]]()

  val boundingRadius:Double = o.bounds.MAX.magnitude

  test(" LeafOctantPR.insert " ) {

    val scalar = (boundingRadius + 0.00001 + Math.random()) * (if (r.nextBoolean()) -1.0 else 1.0)

    var i:Int = 0
    while (i < N) {
      // Generate and test random vectors inside the node:
      val s = o.size + 1
      val v = r.nextVec[3](-50, 50.0)
      o.insert(v)
      all.addOne(v)
      assertEquals(s, o.size)

      // Generate and test outside vectors:
      assert(
        try {
          o.insert(r.nextVec[3]().normalized * scalar)
          false
        } catch {
          case _: Throwable => true
        }
      )

      i += 1
    }

    assert(o.size == all.size)
    assert(o.size == N)
  }

  test(" LeafOctantPR.intersects ") {

    var i:Int = 0
    while (i < N) {
      // Generate and test random vectors inside the node:
      val in = r.nextVec[3](-50, 50.0)
      var tst = o.intersects(in, r.nextDouble() * 50.0)
      if (!tst) println(in.show)
      assertEquals(tst, true)

      def scalar:Double = (boundingRadius + 0.00001 + Math.random()) * (if (r.nextBoolean()) -1.0 else 1.0)
      // Generate and test outside vectors:
      val out = r.nextVec[3]().normalized * scalar
      tst = o.intersects(out, r.nextDouble())
      if (tst) println(out.show)
      assertEquals(tst, false)

      i += 1
    }
  }

  def bruteForceNearestNeighbor(qv: Vec[3]): Vec[3] = {
    var out: Vec[3] = all(0)
    var dist = qv.euclideanDistanceSquaredTo(out)
    var i = 1
    while (i < all.size) {
      val tv = all(i)
      val td = qv.euclideanDistanceSquaredTo(tv)
      if (td < dist) {
        dist = td
        out = tv
      }
      i += 1
    }
    out
  }

  test(" LeafOctantPR.nearestNeighbor ") {

    // compare with brute force method.
    var qvi = 0
    while (qvi < 10) {
      val qv = r.nextVec[3](-50, 50.0)
      val nn = o.nearestNeighbor(qv)
      assertEquals(
        qv.euclideanDistanceSquaredTo(nn),
        qv.euclideanDistanceSquaredTo(bruteForceNearestNeighbor(qv))
      )
      qvi += 1
    }

  }

  test(" LeafOctantPR.minDistanceSquaredTo(qv) ") {
    val lo: OctantPR = new LeafOctantPR(Vec[3](0.0, 0.0, 0.0), 100.0)

    // all sides:
    assert(1.0 == lo.minDistanceSquaredTo(Vec[3](-51.0, 0.0, 0.0)))
    assert(1.0 == lo.minDistanceSquaredTo(Vec[3](0.0, -51.0, 0.0)))
    assert(1.0 == lo.minDistanceSquaredTo(Vec[3](0.0, 0.0, -51.0)))

    assert(1.0 == lo.minDistanceSquaredTo(Vec[3](51.0, 0.0, 0.0)))
    assert(1.0 == lo.minDistanceSquaredTo(Vec[3](0.0, 51.0, 0.0)))
    assert(1.0 == lo.minDistanceSquaredTo(Vec[3](0.0, 0.0, 51.0)))

    // all corners:
    assert(3.0 == lo.minDistanceSquaredTo(Vec[3](51.0, 51.0, 51.0)))
    assert(3.0 == lo.minDistanceSquaredTo(Vec[3](51.0, 51.0, -51.0)))
    assert(3.0 == lo.minDistanceSquaredTo(Vec[3](51.0, -51.0, 51.0)))
    assert(3.0 == lo.minDistanceSquaredTo(Vec[3](51.0, -51.0, -51.0)))
    assert(3.0 == lo.minDistanceSquaredTo(Vec[3](-51.0, 51.0, 51.0)))
    assert(3.0 == lo.minDistanceSquaredTo(Vec[3](-51.0, 51.0, -51.0)))
    assert(3.0 == lo.minDistanceSquaredTo(Vec[3](-51.0, -51.0, 51.0)))
    assert(3.0 == lo.minDistanceSquaredTo(Vec[3](-51.0, -51.0, -51.0)))
  }
  
  def bruteForceRadialQuery(qv: Vec[3], radiusSquared:Double): mutable.HashSet[Vec[3]] = {
    val out: mutable.HashSet[Vec[3]] = mutable.HashSet[Vec[3]]()
    var i = 0
    while (i < all.size) {
      val tv = all(i)
      if (qv.euclideanDistanceSquaredTo(tv) < radiusSquared) out.addOne(tv)
      i += 1
    }
    out
  }

  test(" LeafOctantPR.radialQuery ") {

    // compare with brute force method.
    var qvi = 0
    while (qvi < 10) {
      val qv = r.nextVec[3](-50, 50.0)
      val radiusSquared = slash.squareInPlace(Math.random() * o.bounds.MAX.magnitude)
      val results = o.radialQuery(qv, radiusSquared)
      val bruteForceResults = bruteForceRadialQuery(qv, radiusSquared)

      //println(s"radiusSquared = $radiusSquared found ${results.length} == ${bruteForceResults.size}")
      assert(results.length == bruteForceResults.size)

      for (rv <- results) {
        //println(s"${rv.show}")
        assert(bruteForceResults.contains(rv))
      }

      qvi += 1
    }

  }

}