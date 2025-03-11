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
import slash.vector.*

import scala.collection.mutable

class PROctreeMapTest extends munit.FunSuite {

  val N:Int = 1000

  val otm = new PROctreeMap[String](100.0, Vec[3](0.0, 0.0, 0.0))
  val all: NArrayBuilder[(Vec[3], String)] = NArrayBuilder[(Vec[3], String)]()

  test(" PROctreeMap Insertion ") {
    var i:Int = 0
    while (i < N) {
      val v = r.nextVec[3](-50.0, 50.0)
      if (otm.insert(v, v.show)) {
        all.addOne((v, v.show))
      } else {
        println("insert failed.")
        assert(false)
      }

      i += 1

      assertEquals(otm.size, i)
      assertEquals(all.size, i)
    }
    assertEquals(otm.size, N)

  }

  test(" PROctreeMap.nearestNeighbor ") {

    // compare with brute force method.

    def bruteForceNearestNeighbor(qv:Vec[3]): (Vec[3], String) = {
      var out:(Vec[3], String) = all(0)
      var dist = qv.euclideanDistanceSquaredTo(out._1)
      var i = 1
      while (i < all.size) {
        val tv = all(i)
        val td = qv.euclideanDistanceSquaredTo(tv._1)
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
      val (qv, str) = all(qvi)
      val nn = otm.nearestNeighbor(qv)
      //println(s"self NN ? ${qv.show} ~ ${nn.show}")
      assertEquals(qv, nn._1)
      qvi = qvi + 1
    }

    // nn tests
    qvi = 0
    while (qvi < 10) {
      val qv = r.nextVec[3](-100, 100.0)
      val bfnn = bruteForceNearestNeighbor(qv)
      val nn = otm.nearestNeighbor(qv)
      //println(s"${qv.show} ~ ${bfnn.show} vs ${nn.show}")
      assertEquals(
        qv.euclideanDistanceSquaredTo(nn._1),
        qv.euclideanDistanceSquaredTo(bfnn._1)
      )

      qvi += 1
    }

  }

  test(" PROctreeMap.radialQuery ") {

    // compare with brute force method.

    def bruteForceRadialQuery(qv: Vec[3], radiusSquared: Double): mutable.HashSet[(Vec[3], String)] = {
      val out: mutable.HashSet[(Vec[3], String)] = mutable.HashSet[(Vec[3], String)]()
      var i = 0
      while (i < all.size) {
        val tv = all(i)
        if (qv.euclideanDistanceSquaredTo(tv._1) < radiusSquared) out.addOne(tv)
        i = i + 1
      }
      out
    }

    var qvi = 0
    while (qvi < 10) {
      val qv = r.nextVec[3](-50, 50.0)
      val radius = Math.random() * otm.bounds.MAX.magnitude
      val results = otm.radialQuery(qv, radius)
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

  test(" PROctreeMap.KNN ") {

    def bruteForceKNN(qv: Vec[3], k:Int): mutable.HashSet[(Vec[3], String)] = {
      val tm: mutable.TreeMap[Double, (Vec[3], String)] = new mutable.TreeMap[Double, (Vec[3], String)]()

      var i = 0
      while (i < all.size) {
        val tv = all(i)
        tm.put(qv.euclideanDistanceSquaredTo(tv._1), tv)
        i = i + 1
      }
      val out = new mutable.HashSet[(Vec[3], String)]()
      out.addAll(tm.take(k).values)
      out
    }

    var qvi = 0
    while (qvi < 1000) {
      val K:Int = r.between(2, 11)
      val qv = r.nextVec[3](-50, 50.0)
      val results = otm.KNN(qv, K)
      val bruteForceResults = bruteForceKNN(qv, K)

      if (results.length != bruteForceResults.size) {
        println(s"qv = ${qv.show} and K = $K found ${results.length} == ${bruteForceResults.size}")
        for (rv <- results) print(s"${rv._1.show}")
        println()
        for (bfrv <- bruteForceResults) print(s"${bfrv._1.show}")
        println()
      }

      assert(results.length == bruteForceResults.size)

      var bftd = 0.0
      for (bfrv <- bruteForceResults) {
        bftd = bftd + bfrv._1.euclideanDistanceTo(qv)
        //print(s"${bfrv.show}")
      }
      //println(s"Brute Force Total Distance: $bftd")
      var td = 0.0
      for (rv <- results) {
        td = td + rv._1.euclideanDistanceTo(qv)
        //print(s"${rv.show}")
        assert(bruteForceResults.contains(rv))
      }
      assert(Math.abs(bftd - td) < 0.00001)
      //println(s"KNN Total Distance: $td")

      qvi += 1
    }

  }


}