ThisBuild / scalaVersion := "2.13.6"

ThisBuild / name := "eo-static-analyzer"
ThisBuild / organization := "sitiritis"

lazy val commonSettings = Compiler.settings ++ Seq(
  resolvers += Opts.resolver.sonatypeSnapshots
)

lazy val `eo-static-analyzer-app` = project
  .in(file("."))
  .settings(commonSettings)
  .dependsOn(
    utils,
    core,
    backends,
    parser,
  )
  .aggregate(
    utils,
    core,
    backends,
    parser,
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

lazy val parser = project
  .settings(commonSettings)
  .dependsOn(core)
  .settings(
    name := "parser",
    libraryDependencies ++= Dependencies.parser
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
  // Remove strict checks, so that it is easy to modify sandbox when developing
  .settings(scalacOptions ~= (_.filterNot(Compiler.consoleOptionsToRemove)))
  .dependsOn(`eo-static-analyzer-app`)
  .settings(
    name := "sandbox",
    libraryDependencies ++= Dependencies.common,
  )
