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

import slash.squareInPlace
import slash.vector.{Vec, VectorBounds}

trait Quadrant {

  val center: Vec[2]
  val extent: Double

  lazy val infNorm: Double = extent / 2.0
  private lazy val nCorner = Vec[2](center.x - infNorm, center.y - infNorm)
  private lazy val pCorner = Vec[2](center.x + infNorm, center.y + infNorm)
  lazy val bounds: VectorBounds[2] = VectorBounds[2](nCorner, pCorner)

  def size: Int

  // TODO: replace this method body with an implementation in the next version of slash
  def intersects(v: Vec[2], radiusSquared: Double): Boolean = {
    var distSquared = 0.0

    if (v.x < nCorner.x) distSquared += squareInPlace(v.x - nCorner.x)
    else if (v.x > pCorner.x) distSquared += squareInPlace(v.x - pCorner.x)

    if (v.y < nCorner.y) distSquared += squareInPlace(v.y - nCorner.y)
    else if (v.y > pCorner.y) distSquared += squareInPlace(v.y - pCorner.y)

    distSquared <= radiusSquared
  }

  inline def encompasses(v: Vec[2]): Boolean = bounds.contains(v)

  inline def minDistanceSquaredTo(v: Vec[2]): Double = bounds.minEuclidianDistanceSquaredTo(v)

}
