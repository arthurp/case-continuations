name := "case-continuations"
organization := "org.singingwizard"
version := "0.0.1"

run <<= run in Compile in core

lazy val root = (project in file(".")).
  aggregate(macros, core)
  
lazy val commonSettings = Seq(
  organization := "org.singingwizard",
  version := "0.0.1",
  scalaVersion := "2.11.8",
  autoCompilerPlugins := true,
  addCompilerPlugin("org.scalamacros" % "paradise" % "3.0.0-M3" cross CrossVersion.full)
)

lazy val macros = (project in file("macros")).
  settings(commonSettings: _*).settings(
  //scalacOptions :=
  //  scalacOptions.value :+ ("-Xplugin:/home/amp/.ivy2/cache/org.scalamacros/paradise_2.11.8/jars/paradise_2.11.8-3.0.0-M3.jar")
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.0.0"
)

lazy val core = (project in file("core")).
  settings(commonSettings: _*).settings() dependsOn macros

