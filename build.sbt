name := "case-continuations"
organization := "org.singingwizard"
version := "0.0.1"

scalaVersion in ThisBuild := "2.11.8"

run <<= run in Compile in core

lazy val root = (project in file(".")).
  aggregate(macros, core)

val paradiseVersion = "3.0.0-M3"
  
lazy val commonSettings = Seq(
  organization := "org.singingwizard",
  version := "0.0.1",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-deprecation"),
  addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
)

lazy val macros = (project in file("macros")).
  settings(commonSettings: _*).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val core = (project in file("core")).
  settings(commonSettings: _*).settings(
    scalacOptions ++= Seq("-Xprint:typer")
    //scalacOptions ++= Seq("-uniqid"),
    //scalacOptions ++= Seq("-Xprint-types"),
    //scalacOptions ++= Seq("-Ylog:<phase>"),
    //scalacOptions ++= Seq("-Ylog:typer"),
    //scalacOptions ++= Seq("-Yshow-syms")
  ) dependsOn macros

