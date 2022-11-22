ThisBuild / scalaVersion := "3.2.1"
ThisBuild / publishTo := Some( Resolver.file( "file",  new File("/var/www/maven") ) )

lazy val spatial = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .settings(
    publishTo := Some( Resolver.file( "file",  new File( "/var/www/maven" ) ) ),
    name := "spatial",
    version := "0.4.5321",
    organization := "ai.dragonfly.code",
    resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/",
    scalacOptions ++= Seq("-feature","-deprecation"),
    Compile / mainClass := Some("ai.dragonfly.spatial.OctreeTests"),
    libraryDependencies ++= Seq(
      "ai.dragonfly.code" %%% "vector" % "0.5321"
    )
  ).jvmSettings().jsSettings(
    scalaJSUseMainModuleInitializer := true
  )
