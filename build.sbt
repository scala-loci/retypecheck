name := "retypecheck"

scalaVersion := "2.11.7"

version := "0.0.1-SNAPSHOT"

organization := "de.tuda.stg"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalamacros" %% "resetallattrs" % "1.0.0")
