val appVersion:String = "0.1"
val globalScalaVersion = "3.3.5"

ThisBuild / organization := "ai.dragonfly"
ThisBuild / organizationName := "dragonfly.ai"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List( tlGitHubDev("dragonfly-ai", "dragonfly.ai") )
ThisBuild / scalaVersion := globalScalaVersion

ThisBuild / tlBaseVersion := appVersion
ThisBuild / tlCiReleaseBranches := Seq()

ThisBuild / nativeConfig ~= {
  _.withLTO(scala.scalanative.build.LTO.thin)
    .withMode(scala.scalanative.build.Mode.releaseFast)
    .withGC(scala.scalanative.build.GC.commix)
}

lazy val spatial = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .settings(
    description := "Spatial data structures for Scala JVM, Scala.js, and Scala Native!",
    libraryDependencies ++= Seq( "ai.dragonfly" %%% "slash" % "0.3.2" )
  )
  .jvmSettings()
  .jsSettings()

lazy val root = tlCrossRootProject.aggregate(spatial, tests).settings(name := "spatial")

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin).settings(
  mdocVariables := Map(
    "VERSION" -> appVersion,
    "SCALA_VERSION" -> globalScalaVersion
  ),
  laikaConfig ~= { _.withRawContent }
)

lazy val unidocs = project
  .in(file("unidocs"))
  .enablePlugins(TypelevelUnidocPlugin) // also enables the ScalaUnidocPlugin
  .settings(
    name := "spatial-docs",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(spatial.js, spatial.jvm, spatial.native)
  )

lazy val tests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(spatial)
  .settings(
    name := "spatial-tests",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.0" % Test
  )