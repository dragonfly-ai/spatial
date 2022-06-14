package ai.dragonfly.spatial

import ai.dragonfly.math.vector.*

object OctreeTests extends App {
  // Test node intersection
  val metaNode = new PROctreeMapMetaNode[Int](100.0, Vector3(50.0, 50.0, 50.0), 10, 0, 2)
  println(metaNode.intersects(Vector3(1.0, 1.0, 4.0), 10))
  println(metaNode.intersects(Vector3(100.0, 100.0, 100.0), 9.9999))
  println(metaNode.intersects(Vector3(100.0, 100.0, 100.0), 10))
  println(metaNode.intersects(Vector3(110.0, 110.0, 110.0), 10.1))
  println(metaNode.intersects(Vector3(110.0, 110.0, 110.0), 11))

  // Test Octree
  val ot = new PointRegionOctree[Int](100.0, Vector3(50.0, 50.0, 50.0))
  //for (i <- 0 until 5000000) {
  for (i <- 0 until 10000) {
    ot.insert(
      Vector3(
        Math.random() * 100.0,
        Math.random() * 100.0,
        Math.random() * 100.0
      ),
      (Math.random() * Int.MaxValue).toInt
    )
  }

  val queryVector = Vector3(
    Math.random() * 100.0,
    Math.random() * 100.0,
    Math.random() * 100.0
  )

  val radius = 5
  val radiusSquared = radius * radius

  println(s"Query Vector: $queryVector  Radius: $radius  RadiusSquared: $radiusSquared")

  for ((v, i) <- ot.radialQuery(queryVector, radius)) {
    println(v)
    println(i)
    println(s"$v ${queryVector.distanceTo(v)} ${queryVector.distanceSquaredTo(v)} -> $i")
  }

  println("test nearest neighbor")
  for (i <- 0 until 10) {
    val qv:Vector3 = Vector3(
      Math.random() * 100.0,
      Math.random() * 100.0,
      Math.random() * 100.0
    )

    ot.nearestNeighbor(qv) match {
      case Some((nn, i)) => println(s"$qv's nearest neighbor is: $nn:$i at distance: ${qv.distanceTo(nn)}")
      case None => println(s"$qv has no nearest neighbor?  If $ot.size != 0, you found a bug!")
    }

  }

//  println("Test Iterator:")
//  for ((v: Vector3, i: Int) <- ot.iterator) {
//    println(s"$v -> $i")
//  }

}
