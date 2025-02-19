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

class IntersectionTest extends munit.FunSuite {
  test(" LeafOctant Intersection ") {
    // Test Octree
    val N:Int = 100
    val ot = new LeafOctant[Int](100.0, Vec[3](0.0, 0.0, 0.0), 0, 0, 0)
    var i:Int = 0; while (i < N) {

      // Generate and test random vectors inside the node:
      val in = r.nextVec[3](-100, 100.0)
      var tst = ot.intersects(in, r.nextDouble() * 200.0)
      if (!tst) println(in.show)
      assertEquals(tst, true)

      def randomBoundary:Double = 205 * (if (r.nextBoolean()) -1 else 1)
      // Generate and test outside vectors:
      val out = r.nextVec[3](-100, 100.0) + Vec[3](
        randomBoundary,
        randomBoundary,
        randomBoundary
      )

      tst = ot.intersects(out, r.nextDouble())
      if (tst) println(out.show)
      assertEquals(tst, false)

      i += 1
    }
  }

}