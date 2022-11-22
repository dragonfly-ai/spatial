ThisBuild / scalaVersion := "3.1.0"
ThisBuild / publishTo := Some( Resolver.file( "file",  new File("/var/www/maven") ) )

lazy val spatial = crossProject(JSPlatform, JVMPlatform).settings(
  publishTo := Some( Resolver.file( "file",  new File( "/var/www/maven" ) ) ),
  name := "spatial",
  version := "0.4.53",
  organization := "ai.dragonfly.code",
  resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/",
  scalacOptions ++= Seq("-feature","-deprecation"),
  Compile / mainClass := Some("ai.dragonfly.spatial.OctreeTests"),
  libraryDependencies ++= Seq(
    "ai.dragonfly.code" %%% "vector" % "0.53"
  )
).jvmSettings().jsSettings(
  scalaJSUseMainModuleInitializer := true
)
