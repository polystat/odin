ThisBuild / scalaVersion := "2.13.6"

ThisBuild / name := "eo-static-analyzer"
ThisBuild / organization := "sitiritis"
ThisBuild / version := "0.1"

val commonSettings = Compiler.settings

lazy val utils = project
  .settings(commonSettings)
  .settings(
    name := "utils",
    libraryDependencies ++= Dependencies.common,
  )

lazy val core = project
  .settings(commonSettings)
  .dependsOn(utils)
  .settings(
    name := "core",
    libraryDependencies ++= Dependencies.core,
  )

//lazy val cli = project
//  .dependsOn(core)
//  .settings(
//    name := "eo-static-analyzer",
//    libraryDependencies ++= Dependencies.cli,
//  )

