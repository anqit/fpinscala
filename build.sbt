ThisBuild / organization := "com.ankit"
ThisBuild / version := "2.0.0"
ThisBuild / scalaVersion := "3.1.2"

// uncomment to build with warnings displayed
// set scalacOptions in ThisBuild ++= Seq("-feature", "-deprecation")

lazy val fpinscala = (project in file("."))
  .settings(
      name := "fpinscala",
  )
