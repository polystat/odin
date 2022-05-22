import sbt._

object Dependencies {

  object V {
    val cats = "2.7.0"
    val catsMtl = "1.2.1"
    val catsEffect = "3.3.4"
    val catsParse = "0.3.6"
    val scalaCheck = "3.2.9.0"
    val nonEmpty = "0.2.0"
    val monocle = "3.1.0"
    val droste = "0.8.0"
    val pprint = "0.7.1"
    val fs2 = "3.2.4"
    val fs2io = "3.2.4"
    val newTypes = "0.0.1"
    val munit = "0.7.29"
    val munitScalacheck = "0.7.29"
    val munitScalacheckEffect = "1.0.4"
    val munitCatsEffect = "1.0.7"
    val eoParser = "0.22.2"
    val smtlib = "0.2.1-42-gc68dbaa"
    val princess = "2021-11-15"
  }

  val cats = Seq(
    "org.typelevel" %% "cats-core" % V.cats,
    "org.typelevel" %% "cats-mtl" % V.catsMtl,
  )

  val catsEffect = Seq(
    "org.typelevel" %% "cats-effect" % V.catsEffect,
  )

  val munit = Seq(
    "org.scalameta" %% "munit" % V.munit % Test,
    "org.scalameta" %% "munit-scalacheck" % V.munitScalacheck % Test,
    "org.typelevel" %% "scalacheck-effect-munit" % V.munitScalacheckEffect % Test,
    "org.typelevel" %% "munit-cats-effect-3" % V.munitCatsEffect % Test
  )

  val nonEmpty = Seq(
    "com.github.tarao" %% "nonempty" % V.nonEmpty,
  )

  val monocle = Seq(
    "dev.optics" %% "monocle-core" % V.monocle,
    "dev.optics" %% "monocle-macro" % V.monocle,
  )

  val droste = Seq(
    "io.higherkindness" %% "droste-core" % V.droste
  )

  val fs2 = Seq(
    "co.fs2" %% "fs2-core" % V.fs2,
  )

  val fs2io = Seq(
    "co.fs2" %% "fs2-io" % V.fs2io,
  )

  val newTypes = Seq(
    "io.monix" %% "newtypes-core" % V.newTypes,
  )

  val pprint = Seq(
    "com.lihaoyi" %% "pprint" % V.pprint % Test,
  )

  val catsParse = Seq(
    "org.typelevel" %% "cats-parse" % V.catsParse
  )

  val eoParser = Seq(
    "org.eolang" % "eo-parser" % V.eoParser % Test,
  )

  val smtlib = Seq(
    "com.regblanc" %% "scala-smtlib" % V.smtlib,
    "io.github.uuverifiers" %% "princess" % V.princess
  )

  val allCats: Seq[ModuleID] = cats ++ catsEffect

  val common: Seq[ModuleID] =
    allCats ++ munit ++ nonEmpty ++ monocle ++
      newTypes

  val analysis: Seq[ModuleID] = common ++ smtlib ++ pprint

  val utils: Seq[ModuleID] = common ++ fs2io

  val core: Seq[ModuleID] = common ++ droste ++ fs2

  val parser: Seq[ModuleID] = common ++
    pprint ++
    catsParse ++
    eoParser

  val interop: Seq[ModuleID] = common
}
