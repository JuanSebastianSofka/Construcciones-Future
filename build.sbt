ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "CiudadelaFuturo"
  )

libraryDependencies += "joda-time" % "joda-time" % "2.10.14"
libraryDependencies += "org.joda" % "joda-convert" % "2.2.2"