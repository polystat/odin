package org.polystat.odin.parser.gens

import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams._
import org.scalacheck.Gen

object ast {

  val stringData: Gen[EOStrData[EOExprOnly]] =
    eo.undelimitedString.map(EOStrData[EOExprOnly])

  val charData: Gen[EOCharData[EOExprOnly]] =
    Gen.oneOf('\u0000' to '\u9999').map(EOCharData[EOExprOnly])

  val integerData: Gen[EOIntData[EOExprOnly]] =
    eo.integer.map(int => EOIntData[EOExprOnly](int.toInt))

  val floatData: Gen[EOFloatData[EOExprOnly]] =
    eo.float.map(float => EOFloatData[EOExprOnly](float.toFloat))

  val boolData: Gen[EOBoolData[EOExprOnly]] =
    Gen.oneOf(true, false).map(EOBoolData[EOExprOnly])

  val simpleApp: Gen[EOSimpleApp[EOExprOnly]] =
    Gen
      .oneOf(
        eo.attributeName,
        Gen.const("^"),
        Gen.const("$")
      )
      .map(EOSimpleApp[EOExprOnly])

  val simpleAppWithLocator: Gen[EOSimpleAppWithLocator[EOExprOnly]] =
    Gen
      .choose(0, 5)
      .flatMap(n =>
        eo.identifier.map(name => EOSimpleAppWithLocator[EOExprOnly](name, n))
      )

  val eoData: Gen[EOData[EOExprOnly]] = Gen.oneOf(
    stringData,
    charData,
    integerData,
    floatData,
    boolData,
  )

  val rtMeta: Gen[EORTMeta] =
    for {
      alias <- eo.identifier
      artifact <- eo.artifactName
    } yield EORTMeta(alias, artifact)

  val aliasMeta: Gen[EOAliasMeta] = for {
    alias <- Gen.option(eo.identifier)
    artifact <- eo.packageNameSplit
  } yield EOAliasMeta(alias, artifact)

  val metas: Gen[EOMetas] =
    for {
      pkgMeta <- Gen.option(eo.packageName)
      otherMetas <- between(
        0,
        4,
        Gen.oneOf(
          aliasMeta,
          rtMeta,
        )
      ).map(_.toVector)
    } yield EOMetas(pkgMeta, otherMetas)

  def eoProg(maxDepth: Int): Gen[EOProg[EOExprOnly]] = for {
    metas <- metas
    bnds <- between(
      0,
      4,
      Gen.frequency(
        1 -> anonExpr(maxDepth),
        9 -> bndExpr(maxDepth)
      )
    ).map(_.toVector)
  } yield EOProg(metas, bnds)

  def eoDot(maxDepth: Int, depth: Int = 0): Gen[EODot[EOExprOnly]] = for {
    trg <-
      if (depth < maxDepth)
        eoExpr(maxDepth, depth + 1)
      else
        simpleApp.map(Fix(_))
    name <- eo.attributeName
  } yield EODot(trg, name)

  def eoApp(maxDepth: Int, depth: Int = 0): Gen[EOApp[EOExprOnly]] =
    if (depth < maxDepth)
      Gen.oneOf(
        eoCopy(maxDepth, depth + 1),
        eoDot(maxDepth, depth + 1),
        simpleApp,
        simpleAppWithLocator
      )
    else
      simpleApp

  def eoExpr(maxDepth: Int, depth: Int = 0): Gen[EOExprOnly] = {
    Gen
      .oneOf(eoData, eoApp(maxDepth, depth), eoObj(maxDepth, depth))
      .map(Fix(_))
  }

  val anyNameBnd: Gen[EOAnyNameBnd] = for {
    name <- eo.identifier
    namedBnd <- Gen.oneOf(LazyName(name), ConstName(name))
  } yield EOAnyNameBnd(namedBnd)

  val namedBnd: Gen[EONamedBnd] = Gen.oneOf(anyNameBnd, Gen.const(EODecoration))

  def anonExpr(maxDepth: Int, depth: Int = 0): Gen[EOAnonExpr[EOExprOnly]] =
    eoExpr(maxDepth, depth).map(EOAnonExpr[EOExprOnly])

  def bndExpr(maxDepth: Int, depth: Int = 0): Gen[EOBndExpr[EOExprOnly]] = for {
    expr <- eoExpr(maxDepth, depth)
    namedBnd <- namedBnd
  } yield EOBndExpr(namedBnd, expr)

  def eoCopy(maxDepth: Int, depth: Int = 0): Gen[EOCopy[EOExprOnly]] = {

    for {
      trg <-
        if (depth < maxDepth)
          eoExpr(maxDepth, depth + 1)
        else
          simpleApp.map(Fix(_))
      args <- between(
        min = 1,
        max = 3,
        gen = Gen.frequency(
          (1, bndExpr(maxDepth, depth + 1)),
          (9, anonExpr(maxDepth, depth + 1))
        )
      )
        .flatMap(lst =>
          NonEmpty.from(lst.toVector) match {
            case Some(value) => Gen.const(value)
            case None =>
              anonExpr(maxDepth, depth + 1).map(it =>
                NonEmpty[Vector[EOBnd[EOExprOnly]]](it)
              )
          }
        )
    } yield EOCopy(trg, args)
  }

  def eoObj(maxDepth: Int, depth: Int = 0): Gen[EOObj[EOExprOnly]] = {
    for {
      params <- between(0, 4, eo.paramName).map(_.map(LazyName).toVector)
      vararg <- Gen.option(eo.paramName.map(LazyName))
      bnds <- between(0, 4, bndExpr(maxDepth, depth + 1)).map(_.toVector)
    } yield EOObj(params, vararg, bnds)
  }

}
