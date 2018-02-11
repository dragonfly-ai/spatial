package ai.dragonfly.spacial

import ai.dragonfly.math.vector.Vector3

object TestOctree extends App {

  // Test node intersection
  val metaNode = new PROctreeMapMetaNode[Int](100.0)
  println(metaNode.intersects(Vector3(1.0, 1.0, 4.0), 10))
  println(metaNode.intersects(Vector3(100.0, 100.0, 100.0), 9.9999))
  println(metaNode.intersects(Vector3(100.0, 100.0, 100.0), 10))
  println(metaNode.intersects(Vector3(110.0, 110.0, 110.0), 10.1))
  println(metaNode.intersects(Vector3(110.0, 110.0, 110.0), 11))

  // Test Octree
  val ot = new PointRegionOctree[Int](100.0)
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

  val radius = 5.0
  val radiusSquared = radius * radius

  println(s"Query Vector: $queryVector  Radius: $radius  RadiusSquared: $radiusSquared")

  ot.radialQuery(
    queryVector,
    radius
  ) match {
    case Some(hm) =>
      println(hm)
      for (v <- hm) {
        println(s"$v ${queryVector.distanceTo(v._1)} ${queryVector.distanceSquaredTo(v._1)} -> ${v._2}")
      }
    case None => println("No matches.")
  }


  println("Test Iterator:")
  for ((v: Vector3, i: Int) <- ot.iterator) {
    println(s"$v -> $i")
  }

}
