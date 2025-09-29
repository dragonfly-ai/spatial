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

import ai.dragonfly.spatial.*
import narr.*
import slash.Random.{defaultRandom as r, *}
import slash.vectorf.*

import scala.collection.mutable

class PRQuadTreeFTest extends munit.FunSuite {

  val N:Int = 1000

  val qt = new PRQuadTreeF(100f, VecF[2](0f, 0f))
  val all: NArrayBuilder[VecF[2]] = NArrayBuilder[VecF[2]]()

  test(" PRQuadTree Insertion ") {
    var i:Int = 0
    while (i < N) {
      val v = r.nextVecF[2](-50.0, 50.0)
      if (qt.insert(v)) {
        all.addOne(v)
      } else {
        println("insert failed.")
        assert(false)
      }

      i += 1

      assertEquals(qt.size, i)
      assertEquals(all.size, i)
    }
    assertEquals(qt.size, N)

  }

  test(" PRQuadTree.nearestNeighbor ") {

    // compare with brute force method.

    def bruteForceNearestNeighbor(qv:VecF[2]): VecF[2] = {
      var out:VecF[2] = all(0)
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

    // nn exact match tests
    var qvi = 0
    while (qvi < all.size) {
      val qv = all(qvi)
      val nn = qt.nearestNeighbor(qv)
      //println(s"self NN ? ${qv.show} ~ ${nn.show}")
      assertEquals(qv, nn)
      qvi = qvi + 1
    }

    // nn tests
    qvi = 0
    while (qvi < 10) {
      val qv = r.nextVecF[2](-100, 100.0)
      val bfnn = bruteForceNearestNeighbor(qv)
      val nn = qt.nearestNeighbor(qv)
      //println(s"${qv.show} ~ ${bfnn.show} vs ${nn.show}")
      assertEquals(
        qv.euclideanDistanceSquaredTo(nn),
        qv.euclideanDistanceSquaredTo(bfnn)
      )

      qvi += 1
    }

  }

  test(" PRQuadTree.radialQuery ") {

    // compare with brute force method.

    def bruteForceRadialQuery(qv: VecF[2], radiusSquared: Float): mutable.HashSet[VecF[2]] = {
      val out: mutable.HashSet[VecF[2]] = mutable.HashSet[VecF[2]]()
      var i = 0
      while (i < all.size) {
        val tv = all(i)
        if (qv.euclideanDistanceSquaredTo(tv) < radiusSquared) out.addOne(tv)
        i = i + 1
      }
      out
    }

    var qvi = 0
    while (qvi < 10) {
      val qv = r.nextVecF[2](-50, 50.0)
      val radius = (Math.random() * qt.bounds.MAX.magnitude).toFloat
      val results = qt.radialQuery(qv, radius)
      val bruteForceResults = bruteForceRadialQuery(qv, slash.squareInPlace(radius))

      //println(s"radiusSquared = $radius found ${results.length} == ${bruteForceResults.size}")
      assert(results.length == bruteForceResults.size)

      for (rv <- results) {
        //println(s"${rv.show}")
        assert(bruteForceResults.contains(rv))
      }

      qvi += 1
    }

  }

  test(" PRQuadTree.knn ") {

    def bruteForceKNN(qv: VecF[2], k:Int): mutable.HashSet[VecF[2]] = {
      val tm: mutable.TreeMap[Double, VecF[2]] = new mutable.TreeMap[Double, VecF[2]]()

      var i = 0
      while (i < all.size) {
        val tv = all(i)
        tm.put(qv.euclideanDistanceSquaredTo(tv), tv)
        i = i + 1
      }
      val out = new mutable.HashSet[VecF[2]]()
      out.addAll(tm.take(k).values)
      out
    }

    var qvi = 0
    while (qvi < 1000) {
      val K:Int = r.between(2, 11)
      val qv = r.nextVecF[2](-50, 50.0)
      val results = qt.knn(qv, K)
      val bruteForceResults = bruteForceKNN(qv, K)

      if (results.length != bruteForceResults.size) {
        println(s"qv = ${qv.show} and K = $K found ${results.length} == ${bruteForceResults.size}")
        for (rv <- results) print(s"${rv.show}")
        println()
        for (bfrv <- bruteForceResults) print(s"${bfrv.show}")
        println()
      }

      assert(results.length == bruteForceResults.size)

      var bftd = 0.0
      for (bfrv <- bruteForceResults) {
        bftd = bftd + bfrv.euclideanDistanceTo(qv)
        //print(s"${bfrv.show}")
      }
      //println(s"Brute Force Total Distance: $bftd")
      var td = 0.0
      for (rv <- results) {
        td = td + rv.euclideanDistanceTo(qv)
        //print(s"${rv.show}")
        assert(bruteForceResults.contains(rv))
      }
      assert(Math.abs(bftd - td) < 0.00001)
      //println(s"knn Total Distance: $td")

      qvi += 1
    }

  }

}