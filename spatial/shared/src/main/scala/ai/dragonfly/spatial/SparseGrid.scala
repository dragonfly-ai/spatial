package ai.dragonfly.spatial

import scala.collection.mutable

class SparseGrid[T](width:Int, depth:Int, height:Int, widthStep:Double, depthStep:Double, heightStep:Double, minX:Double = 0.0, minY:Double = 0.0, minZ:Double = 0.0) {

  private val cells:mutable.HashMap[Int, mutable.HashSet[T]] = mutable.HashMap[Int, mutable.HashSet[T]]()

  private val widthXdepth:Int = width * depth

  inline def linearIndexOf(x: Double, y: Double, z: Double): Int = {
    var out = -1
    val xi:Int = ((x - minX) / widthStep).toInt
    if (-1 < xi && xi < width) {
      val yi:Int = ((y - minY) / depthStep).toInt
      if (-1 < yi && yi < depth) {
        val zi:Int = ((z - minZ) / heightStep).toInt
        if (-1 < zi && zi < height) {
          out = xi + (yi * width) + (zi * widthXdepth)
        }
      }
    }
    out
  }

}
