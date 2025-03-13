&nbsp;&nbsp;&nbsp;<b>Spatial</b> provides Scala with space partitioning data structures, currently: Point Region Octree `PROctree`, `PRQuadTree`, `PROctreeMap[T]`, and `PRQuadTreeMap[T]`.
This library cross compiles to all Scala compilation targets: JVM, Native, and Scala.js.

Design Goals:
<ol>
<li>Cross compile to JVM, Native, and JavaScript platforms</li>
<li>Maximize performance</li>
<li>Minimize memory footprint</li>
<li>Provide convenient syntax</li>
<li>Seamlessly interoperate with other libraries</li>
<li>Seamlessly interoperate with native languages like JavaScript, C/C++, and Java</li>
</ol>

&nbsp;&nbsp;&nbsp;To meet these design goal, Spatial relies on [SLASH](https://github.com/dragonfly-ai/slash) for its vector formats.  TLDR, that means that regardless of platform, it uses the lightest, fastest, and most portable possible vector format in the entire Scala ecosystem.

&nbsp;&nbsp;&nbsp;To circumvent collections overhead, Spatial relies on [NArr](https://github.com/dragonfly-ai/narr) and its highly optimized `NArrayBuilder[T]`.  This simultaneously minimizes memory footprint, maximizes speed, and maximizes platform interop. 

&nbsp;&nbsp;&nbsp;Spatial provides data structures optimized for efficient population and querying.  Concerns like thread safety and standard Scala collections semantics go partially supported or, in cases like element removal, completely ignored.
We make it efficient and easy to build QuadTrees and Octrees and then perform nearest neighbor, k-nearest neighbor, and radial searches on them.
Instead of removing nodes, though, we recommend building a new data structure from scratch with the removed nodes omitted.
Instead of iterating over a spatial data structure as one does with normal scala collections, we recommend storing references to the same points in another collection for filtering, mapping, traversal, etc.

We aspire to add support for k-d trees in the near future, and variants of Octree and QuadTree that support volumes, not just points.  Pull requests welcome!

To use this library with SBT:

```scala
libraryDependencies += "ai.dragonfly" %%% "spatial" % "<LATEST_VERSION>"
```

How to use `PRQuadTree`:
```scala
// construction
val qt = new PRQuadTree(
  100.0,            // The extent/length/width of this QuadTree
  Vec[2](0.0, 0.0)  // the centroid
)

// insertion:
val b: Boolean = qt.insert(Vec[2](25.0, 25.0))

// nearest neighbor search:
val nn: Vec[2] = qt.nearestNeighbor(Vec[2](24.0, 23.0))

// k nearest neighbor search:
val knn: NArray[Vec[2]] = qt.knn(Vec[2](24.0, 23.0))

// radial query:
val radialResults: NArray[Vec[2]] = qt.radialQuery(Vec[2](0.0, 0.0), 42.0)

// test if a point lies within the boundary of the QuadTree:
val isInside:Boolean = qt.encompasses(Vec[2](42.0, 42.0))

// cardinality:
val s:Int = qt.size

// bounds:
val bnds:slash.vector.VecBounds[2] = qt.bounds
```

How to use `PRQuadTreeMap[T]`:
```scala
// construction
val qtm = new PRQuadTreeMap[String](
  100.0,            // The extent/length/width of this QuadTree
  Vec[2](0.0, 0.0)  // the centroid
)

// insertion:
val b: Boolean = qtm.insert(Vec[2](25.0, 25.0), "QuadTreeMap Love!")

// nearest neighbor search:
val nn: (Vec[2], String) = qtm.nearestNeighbor(Vec[2](24.0, 23.0))

// k nearest neighbor search:
val knn: NArray[(Vec[2], String)] = qtm.knn(Vec[2](24.0, 23.0))

// radial query:
val radialResults: NArray[(Vec[2], String)] = qtm.radialQuery(Vec[2](0.0, 0.0), 42.0)

// test if a point lies within the boundary of the QuadTree:
val isInside:Boolean = qtm.encompasses(Vec[2](42.0, 42.0))

// cardinality:
val s:Int = qtm.size

// bounds:
val bnds:slash.vector.VecBounds[2] = qtm.bounds
```

How to use `PROctree`:
```scala
// construction
val ot = new PROctree(
  100.0,            // The extent/length/width of this QuadTree
  Vec[3](0.0, 0.0, 0.0)  // the centroid
)

// insertion:
val b: Boolean = ot.insert(Vec[3](25.0, 25.0, 25.0))

// nearest neighbor search:
val nn: Vec[3] = ot.nearestNeighbor(Vec[3](24.0, 23.0, 26.0))

// k nearest neighbor search:
val knn: NArray[Vec[3]] = ot.knn(Vec[3](24.0, 23.0, 26.0))

// radial query:
val radialResults: NArray[Vec[3]] = ot.radialQuery(Vec[3](0.0, 0.0, 0.0), 42.0)

// test if a point lies within the boundary of the Point Region Octree:
val isInside:Boolean = ot.encompasses(Vec[3](42.0, 42.0, 42.0))

// cardinality:
val s:Int = ot.size

// bounds:
val bnds:slash.vector.VecBounds[3] = ot.bounds
```

How to use `PROctreeMap[T]`:
```scala
// construction
val otm = new PROctreeMap[String](
  100.0,            // The extent/length/width of this QuadTree
  Vec[3](0.0, 0.0, 0.0)  // the centroid
)

// insertion:
val b: Boolean = otm.insert(Vec[3](25.0, 25.0, 25.0), "PROctreeMap Love!")

// nearest neighbor search:
val nn: (Vec[3], String) = otm.nearestNeighbor(Vec[3](24.0, 23.0, 26.0))

// k nearest neighbor search:
val knn: NArray[(Vec[3], String)] = otm.knn(Vec[3](24.0, 23.0, 26.0))

// radial query:
val radialResults: NArray[(Vec[3], String)] = otm.radialQuery(Vec[3](0.0, 0.0, 0.0), 42.0)

// test if a point lies within the boundary of the Point Region Octree Map:
val isInside:Boolean = otm.encompasses(Vec[3](42.0, 42.0))

// cardinality:
val s:Int = otm.size

// bounds:
val bnds:slash.vector.VecBounds[3] = otm.bounds
```

Projects that rely on Spatial:

https://github.com/dragonfly-ai/bitfrost

https://github.com/dragonfly-ai/beacon