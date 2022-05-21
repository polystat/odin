import ReleaseTransformations._
import sbt.addCompilerPlugin

ThisBuild / scalaVersion := "2.13.6"

ThisBuild / crossScalaVersions ++= Seq(
  "2.13.6",
  "3.1.2",
)

ThisBuild / name := "odin-project"
ThisBuild / organization := "org.polystat.odin"
ThisBuild / organizationName := "org.polystat"
ThisBuild / organizationHomepage := Some(url("https://github.com/polystat"))
ThisBuild / homepage := Some(url("https://github.com/polystat/odin"))

ThisBuild / description :=
  """Odin (object dependency inspector) — static analyzer for EO source code
    |that detects OOP-related bugs.""".stripMargin

ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / sonatypeProfileName := "org.polystat"
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / releaseVersionBump := sbtrelease.Version.Bump.Next
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixScalaBinaryVersion := "2.13"
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

lazy val publishSettings = Seq(
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/polystat/odin"),
      "scm:git@github.com:polystat/odin.git"
    )
  ),
  developers := List(
    Developer(
      id = "sitiritis",
      name = "Tymur Lysenko",
      email = "nanotimcool@gmail.com",
      url = url("https://github.com/Sitiritis"),
    ),
    Developer(
      id = "nikololiahim",
      name = "Mihail Olokin",
      email = "olomishcak@gmail.com",
      url = url("https://github.com/nikololiahim"),
    ),
  ),
  licenses := List("MIT" -> url("https://mit-license.org")),
  pomIncludeRepository := { _ => false },
  publishTo := sonatypePublishToBundle.value,
  publishArtifact := true,
  publishMavenStyle := true,
  sonatypeProfileName := "org.polystat",
  releasePublishArtifactsAction := PgpKeys.publishSigned.value
)

lazy val commonSettings = Compiler.settings ++ Seq(
  resolvers += Opts.resolver.sonatypeSnapshots
)


val checkoutSetupJava = List(WorkflowStep.Checkout) ++
  WorkflowStep.SetupJava(List(JavaSpec.temurin("11")))

ThisBuild / githubWorkflowPublishTargetBranches := Seq()

ThisBuild / githubWorkflowAddedJobs ++= Seq(
  WorkflowJob(
    id = "scalafmt",
    name = "Format code with scalafmt",
    scalas = List("2.13.6"),
    steps = checkoutSetupJava ++
      githubWorkflowGeneratedCacheSteps.value ++
      List(
        WorkflowStep.Sbt(List("scalafmtCheckAll")),
      ),
  ),
  WorkflowJob(
    id = "scalafix",
    name = "Check code with scalafix",
    scalas = List("2.13.6"),
    steps = checkoutSetupJava ++
      githubWorkflowGeneratedCacheSteps.value ++
      List(WorkflowStep.Sbt(List("scalafixAll --check"))),
  ),
)

releaseCrossBuild := true
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  releaseStepCommand("sonatypeBundleRelease"),
  setNextVersion,
  commitNextVersion,
  pushChanges,
)

lazy val odin = project
  .in(file("."))
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(
    utils,
    core,
    parser,
    analysis,
    backends,
    interop,
  )
  .aggregate(
    utils,
    core,
    parser,
    analysis,
    backends,
    interop,
  )
  .settings(
    name := "odin",
  )

lazy val utils = project
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "utils",
    libraryDependencies ++= Dependencies.utils,
  )

lazy val core = project
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(utils)
  .settings(
    name := "core",
    libraryDependencies ++= Dependencies.core,
  )

lazy val parser = project
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(
    core,
    `eolang-backend`
  )
  .settings(
    name := "parser",
    libraryDependencies ++= Dependencies.parser
  )

lazy val analysis = project
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(
    parser,
    core
  )
  .settings(
    name := "analysis",
    libraryDependencies ++= Dependencies.analysis
  )

val backendsBaseDirectory: File = file("backends")

lazy val backends: Project = project
  .in(backendsBaseDirectory)
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(`eolang-backend`)
  .aggregate(`eolang-backend`)
  .settings(
    name := "backends",
  )

lazy val `eolang-backend` = project
  .in(backendsBaseDirectory / "eolang")
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(core)
  .settings(
    name := "eolang-backend",
  )

lazy val interop = project
  .settings(commonSettings)
  .settings(publishSettings)
  .dependsOn(
    analysis
  )
  .settings(
    name := "interop",
    libraryDependencies ++= Dependencies.interop
  )

lazy val sandbox = project
  .settings(commonSettings)
  // Remove strict checks, so that it is easy to modify sandbox when developing
  .settings(scalacOptions ~= (_.filterNot(Compiler.consoleOptionsToRemove)))
  .settings(noPublishSettings)
  .dependsOn(`odin`)
  .settings(
    name := "sandbox",
    libraryDependencies ++= Dependencies.common,
  )
