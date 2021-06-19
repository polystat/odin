ThisBuild / scalaVersion := "2.13.6"

ThisBuild / name := "eo-static-analyzer"
ThisBuild / organization := "sitiritis"
ThisBuild / version := "0.1"

val commonSettings = Compiler.settings

lazy val `eo-static-analyzer-app` = project
  .in(file("."))
  .settings(commonSettings)
  .dependsOn(
    utils,
    core,
    backends,
  )
  .aggregate(
    utils,
    core,
    backends,
  )
  .settings(
    name := "eo-static-analyzer-app",
  )

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

val backendsBaseDirectory: File = file("backends")
lazy val backends: Project = project
  .in(backendsBaseDirectory)
  .settings(commonSettings)
  .dependsOn(`eolang-backend`)
  .aggregate(`eolang-backend`)
  .settings(
    name := "backends",
  )

lazy val `eolang-backend` = project
  .in(backendsBaseDirectory / "eolang")
  .settings(commonSettings)
  .dependsOn(core)
  .settings(
    name := "eolang-backend",
  )

lazy val sandbox = project
  .settings(commonSettings)
  .dependsOn(`eo-static-analyzer-app`)
  .settings(
    name := "sandbox",
    libraryDependencies ++= Dependencies.common,
  )

//lazy val cli = project
//  .dependsOn(core)
//  .settings(
//    name := "eo-static-analyzer",
//    libraryDependencies ++= Dependencies.cli,
//  )
