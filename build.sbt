name := "case-continuations"
organization := "org.singingwizard"
version := "0.0.1"

scalaVersion in ThisBuild := "2.11.8"

  
lazy val macros = (project in file("macros")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val core = (project in file("core")).settings(
  scalacOptions ++= Seq()
  ) dependsOn macros

