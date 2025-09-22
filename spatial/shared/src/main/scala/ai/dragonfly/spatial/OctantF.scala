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

import slash.*
import slash.vectorf.*
import slash.vectorf.vectorf.*

trait OctantF {

  val center: VecF[3]
  val extent: Float

  // infNorm: half side length, L∞ norm (max distance from center to face)
  lazy val infNorm: Float = extent / 2f
  private lazy val nCorner = VecF[3](center.x - infNorm, center.y - infNorm, center.z - infNorm)
  private lazy val pCorner = VecF[3](center.x + infNorm, center.y + infNorm, center.z + infNorm)
  lazy val bounds: VectorFBounds[3] = VectorFBounds[3](nCorner, pCorner)
  //private lazy val boundingRadius = nCorner.euclideanDistanceTo(pCorner) / 2.0

  def size: Int

  def intersects(v: VecF[3], radiusSquared: Float): Boolean = bounds.intersectsSphere(v, radiusSquared)

  inline def encompasses(v: VecF[3]): Boolean = bounds.contains(v)

  inline def minDistanceSquaredTo(v: VecF[3]): Double = bounds.minEuclidianDistanceSquaredTo(v)

}
