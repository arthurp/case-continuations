name := "case-continuations"
organization := "org.singingwizard"
version := "0.0.1"

scalaVersion in ThisBuild := "2.11.8"

run <<= run in Compile in core

lazy val root = (project in file(".")).
  aggregate(macros, core)
  
lazy val commonSettings = Seq(
  organization := "org.singingwizard",
  version := "0.0.1",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-deprecation"),
  autoCompilerPlugins := true,
  addCompilerPlugin("org.scalamacros" % "paradise" % "3.0.0-M3" cross CrossVersion.full)
)

lazy val macros = (project in file("macros")).
  settings(commonSettings: _*).settings(
    libraryDependencies += "org.scalameta" %% "scalameta" % "1.0.0"
    //libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val core = (project in file("core")).
  settings(commonSettings: _*).settings(
    scalacOptions ++= Seq("-Xprint:typer")
    //scalacOptions ++= Seq("-uniqid"),
    //scalacOptions ++= Seq("-Yshow-syms")
  ) dependsOn macros

