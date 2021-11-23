import sbt._

object Dependencies {

  object V {
    val cats = "2.6.1"
    val catsMtl = "1.2.1"
    val catsEffect = "3.2.8"
    val scalaTest = "3.2.9"
    val scalaCheck = "3.2.9.0"
    val nonEmpty = "0.2.0"
    val monocle = "3.1.0"
    val parserCombinators = "2.0.0"
    val droste = "0.8.0"
    val fastparse = "2.3.3"
    val pprint = "0.6.6"
    val fs2 = "3.2.2"
    val fs2io = "3.2.2"
    val newTypes = "0.0.1"
    val eoParser = "0.16.7"
    val catsEffectScalatest = "1.3.0"
  }

  val cats = Seq(
    "org.typelevel" %% "cats-core" % V.cats,
    "org.typelevel" %% "cats-mtl" % V.catsMtl,
  )

  val catsEffect = Seq(
    "org.typelevel" %% "cats-effect" % V.catsEffect,
    "org.typelevel" %% "cats-effect-testing-scalatest" % V.catsEffectScalatest % Test,
  )

  val scalaTest = Seq(
    "org.scalactic" %% "scalactic" % V.scalaTest % Test,
    "org.scalatest" %% "scalatest" % V.scalaTest % Test,
    "org.scalatestplus" %% "scalacheck-1-15" % V.scalaCheck % Test,
  )

  val nonEmpty = Seq(
    "com.github.tarao" %% "nonempty" % V.nonEmpty,
  )

  val monocle = Seq(
    "dev.optics" %% "monocle-core" % V.monocle,
    "dev.optics" %% "monocle-macro" % V.monocle,
  )

  val parserCombinators = Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % V.parserCombinators,
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

  val fastparse = Seq(
    "com.lihaoyi" %% "fastparse" % V.fastparse,
  )

  val pprint = Seq(
    "com.lihaoyi" %% "pprint" % V.pprint % Test,
  )

  val eoParser = Seq(
    "org.eolang" % "eo-parser" % V.eoParser % Test,
  )

  val allCats: Seq[ModuleID] = cats ++ catsEffect

  val common: Seq[ModuleID] = allCats ++ scalaTest ++ nonEmpty ++ monocle ++
    newTypes

  val utils: Seq[ModuleID] = common ++ fs2io

  val core: Seq[ModuleID] = common ++ droste ++ fs2

  val parser: Seq[ModuleID] = common ++
    parserCombinators ++
    fastparse ++
    pprint ++
    eoParser

  val interop: Seq[ModuleID] = common
}
