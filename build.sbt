name := "case-continuations"
organization := "org.singingwizard"
version := "0.0.1"

autoCompilerPlugins := true

scalaVersion in ThisBuild := "2.11.8"
run <<= run in Compile in core

scalacOptions :=
  scalacOptions.value :+ ("-Xplugin:/home/amp/.ivy2/cache/org.scalamacros/paradise_2.11.8/jars/paradise_2.11.8-3.0.0-M3.jar")

lazy val macros = (project in file("macros")).settings {
  //autoCompilerPlugins := true
  //addCompilerPlugin("org.scalamacros" % "paradise" % "3.0.0-M3" cross CrossVersion.full)
  scalacOptions :=
    scalacOptions.value :+ ("-Xplugin:/home/amp/.ivy2/cache/org.scalamacros/paradise_2.11.8/jars/paradise_2.11.8-3.0.0-M3.jar")
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.0.0"
}

lazy val core = (project in file("core")).settings { 
  autoCompilerPlugins := true
  addCompilerPlugin("org.scalamacros" % "paradise" % "3.0.0-M3" cross CrossVersion.full)
} dependsOn macros
