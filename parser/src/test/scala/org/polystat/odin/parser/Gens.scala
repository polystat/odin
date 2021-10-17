package org.polystat.odin.parser

import org.scalacheck.Gen

object Gens {

  def between[T](
    min: Int,
    max: Int,
    gen: Gen[T],
    sep: Gen[String] = ""
  ): Gen[String] = {
    for {
      len <- Gen.choose(min, max)
      gens <- Gen.listOfN(len, gen)
      sep <- sep
    } yield gens.mkString(sep)
  }

  def surroundedBy[T, S](gen: Gen[T], sur: Gen[S]): Gen[String] = for {
    before <- sur
    thing <- gen
    after <- sur
  } yield s"$before$thing$after"

  val wsp: Gen[String] =
    between(
      1,
      3,
      Gen.frequency(
        (1, "\t"),
        (19, " ")
      )
    ).map(_.mkString)

  val optWsp: Gen[String] = between(
    0,
    2,
    Gen.frequency(
      (1, "\t"),
      (9, " ")
    )
  )

  val eol: Gen[String] = for {
    before <- optWsp
    le <- Gen.oneOf("\n", "\r\n")
  } yield (before :: le :: Nil).mkString

  val smallLetter: Gen[Char] = Gen.alphaLowerChar
  val letter: Gen[Char] = Gen.alphaChar

  val emptyLinesOrComments: Gen[String] = {
    val emptyLine = for {
      wsp <- optWsp
      eol <- eol
    } yield s"$wsp$eol"
    val comment = for {
      before <- optWsp
      comment <- between(0, 15, Gen.alphaLowerChar)
      eol <- eol
    } yield s"$before#$comment$eol"
    between(0, 3, Gen.oneOf(emptyLine, comment))
  }

  val digit: Gen[Char] = Gen.numChar
  val digits: Gen[String] = between(1, 5, digit)
  val nonZeroDigit: Gen[Char] = Gen.oneOf('1' to '9')

  val nonZeroInteger: Gen[String] = for {
    sign <- Gen.oneOf("", "-")
    first <- nonZeroDigit
    rest <- between(0, 1, digits)
  } yield (sign :: first :: rest :: Nil).mkString

  val integer: Gen[String] = Gen.frequency(
    (1, "0"),
    (99, nonZeroInteger)
  )

  val float: Gen[String] = for {
    before <- integer
    after <- digits
  } yield s"$before.$after"

  val escapedUnicode: Gen[String] = between(4, 4, digit).map("\\u" + _)

  val javaEscape: Gen[String] = Gen.frequency(
    (1, "\\t"),
    (1, "\\b"),
    (1, "\\n"),
    (1, "\\r"),
    (1, "\\f"),
    (1, "\\\'"),
    (1, "\\\""),
    (1, "\\\\"),
  )

  val undelimitedChar: Gen[String] = Gen.frequency(
    (1, escapedUnicode),
    (1, javaEscape),
    (
      8,
      Gen
        .asciiPrintableChar
        .retryUntil(c => c != '"' && c != '\\')
        .map(_.toString)
    )
  )

  val string: Gen[String] =
    between(
      0,
      15,
      undelimitedChar
    ).map(str => s"\"$str\"")

  val char: Gen[String] = undelimitedChar
    .retryUntil(_ != "'")
    .map(c => s"'$c'")

  val identifierChar: Gen[Char] =
    Gen.frequency(
      (5, smallLetter),
      (5, letter),
      (2, Gen.numChar),
      (1, '_'),
      (1, '-')
    )

  val identifier: Gen[String] = for {
    fst <- smallLetter
    rest <- between(0, 3, identifierChar)
  } yield fst +: rest

  val packageName: Gen[String] =
    between(1, 3, identifier, sep = ".")

  val packageMeta: Gen[String] = for {
    name <- packageName
    wsp <- wsp
  } yield s"+package$wsp$name"

  val aliasMeta: Gen[String] = for {
    alias <- surroundedBy(identifier, wsp)
    pkg <- packageName
  } yield s"+alias$alias$pkg"

  val artifactName: Gen[String] = for {
    pkgName <- packageName
    artifactName <- identifier
    version <-
      between(3, 3, digits.retryUntil(s => !s.startsWith("0")), sep = ".")
  } yield s"$pkgName:$artifactName:$version"

  val rtMeta: Gen[String] = for {
    alias <- surroundedBy(identifier, wsp)
    artifactId <- artifactName
  } yield s"+rt$alias$artifactId"

  val metas: Gen[String] = for {
    header <- emptyLinesOrComments
    pkg <- between(0, 1, packageMeta)
    pkgEol <- eol
    metas <- between(
      0,
      5,
      for {
        comments <- emptyLinesOrComments
        meta <- Gen.oneOf(rtMeta, aliasMeta)
        metaEol <- eol
      } yield (comments :: meta :: metaEol :: Nil).mkString
    )
  } yield (header :: pkg :: pkgEol :: metas :: Nil).mkString

  val paramName: Gen[String] = Gen.frequency(
    (9, identifier),
    (1, "@")
  )

  val bndName: Gen[String] = for {
    op <- surroundedBy(">", optWsp)
    id <- paramName
    exclamationMark <- surroundedBy(between(0, 1, "!"), optWsp)
  } yield (op :: id :: exclamationMark :: Nil).mkString

  val abstractionParams: Gen[String] = for {
    params <- between(0, 5, paramName, sep = wsp)
    vararg <- between(0, 1, if (params.nonEmpty) "..." else "")
  } yield s"[$params$vararg]"

  val attributeName: Gen[String] = Gen.frequency(
    (1, "@"),
    (1, "$"),
    (1, "^"),
    (10, identifier)
  )

  val data: Gen[String] = Gen.oneOf(
    integer,
    float,
    string,
    char
  )

  def singleLineApplication(
    recDepthMax: Int,
    recDepth: Int = 0,
  ): Gen[String] = {

    val simpleApplicationTarget = Gen.oneOf(
      data,
      attributeName
    )

    val attributeChain = for {
      trg <- simpleApplicationTarget
      attrs <- between(1, 3, attributeName, sep = ".")
    } yield s"$trg.$attrs"

    val applicationTarget = Gen.oneOf(
      simpleApplicationTarget,
      attributeChain
    )

    val parenthesized = Gen.lzy(
      singleLineApplication(
        recDepth = recDepth + 1,
        recDepthMax = recDepthMax
      )
        .map(s => s"($s)")
    )

    val horizontalApplicationArgs = {
      val arg =
        if (recDepth < recDepthMax)
          Gen.oneOf(parenthesized, applicationTarget)
        else {
          applicationTarget
        }
      between(1, 5, arg, sep = wsp)
    }

    val justApplication = for {
      trg <-
        if (recDepth < recDepthMax)
          Gen.oneOf(parenthesized, applicationTarget)
        else applicationTarget
      sep <- wsp
      args <- horizontalApplicationArgs
    } yield (trg :: sep :: args :: Nil).mkString

    val singleLineArray = for {
      sep <- wsp
      elem =
        if (recDepth < recDepthMax)
          Gen.oneOf(parenthesized, applicationTarget)
        else applicationTarget
      elems <- between(0, 4, elem, sep = wsp)
    } yield s"*$sep$elems"

    Gen.oneOf(
      justApplication,
      applicationTarget,
      parenthesized,
      singleLineArray
    )
  }

  val nothing: Gen[String] = Gen.oneOf("" :: Nil)

  def wspBetweenObjs(
    recDepth: Int,
    indentationStep: Int
  ): Gen[String] = for {
    le <- eol
    comments <- emptyLinesOrComments
  } yield (le :: comments :: (" " * (recDepth * indentationStep)) :: Nil).mkString

  def `object`(
    named: Boolean,
    indentationStep: Int,
    recDepthMax: Int,
    recDepth: Int = 0,
    includeInverseDot: Boolean = true,
  ): Gen[String] = {
    val abstraction = for {
      params <- abstractionParams
      name <- if (named) bndName else nothing
      ifAttrs <- Gen.oneOf(true, false)
      attrs <-
        if (recDepth < recDepthMax && ifAttrs)
          boundAttributes(
            recDepth = recDepth + 1,
            indentationStep = indentationStep,
            recDepthMax = recDepthMax,
          )
        else nothing
    } yield (params :: name :: attrs :: Nil).mkString

    val inverseDotApplication = for {
      id <- identifier
      name <- if (named) bndName else nothing
      attrs <- verticalApplicationArgs(
        recDepth = recDepth + 1,
        indentationStep = indentationStep,
        recDepthMax = recDepthMax,
        includeInverseDot = recDepth < recDepthMax
      )
    } yield (id :: "." :: name :: attrs :: Nil).mkString

    val regularApplication = for {
      trg <- singleLineApplication(recDepthMax = recDepthMax)
      name <- if (named) bndName else nothing
      ifArgs <- Gen.oneOf(true, false)
      args <-
        if (recDepth < recDepthMax && ifArgs)
          verticalApplicationArgs(
            recDepth = recDepth + 1,
            indentationStep = indentationStep,
            recDepthMax = recDepthMax,
          )
        else nothing
    } yield (trg :: name :: args :: Nil).mkString

    val verticalArray = for {
      name <- if (named) bndName else nothing
      ifHasItems <- Gen.oneOf(true, false)
      items <-
        if (recDepth < recDepthMax && ifHasItems)
          verticalApplicationArgs(
            recDepth = recDepth + 1,
            indentationStep = indentationStep,
            recDepthMax = recDepthMax,
          )
        else nothing
    } yield ("*" :: name :: items :: Nil).mkString

    Gen.oneOf(
      abstraction,
      regularApplication,
      if (includeInverseDot) inverseDotApplication else regularApplication,
      verticalArray,
    )

  }

  def verticalApplicationArgs(
    recDepth: Int,
    indentationStep: Int,
    recDepthMax: Int,
    includeInverseDot: Boolean = true,
  ): Gen[String] = for {
    before <- wspBetweenObjs(recDepth, indentationStep)
    named <- Gen.oneOf(true, false)
    objs <- between(
      1,
      5,
      `object`(
        recDepth = recDepth,
        named = named,
        indentationStep = indentationStep,
        recDepthMax = recDepthMax,
        includeInverseDot = includeInverseDot
      ),
      sep = wspBetweenObjs(recDepth, indentationStep)
    )
  } yield (before :: objs :: Nil).mkString

  def boundAttributes(
    recDepth: Int,
    indentationStep: Int,
    recDepthMax: Int,
  ): Gen[String] = for {
    before <- wspBetweenObjs(recDepth, indentationStep)
    objs <- between(
      1,
      5,
      `object`(
        recDepth = recDepth,
        named = true,
        indentationStep = indentationStep,
        recDepthMax = recDepthMax
      ),
      sep = wspBetweenObjs(recDepth, indentationStep)
    )
  } yield (before :: objs :: Nil).mkString

  def program(
    indentationStep: Int,
    recDepthMax: Int,
  ): Gen[String] = for {
    metas <- metas
    objs <- between(
      0,
      3,
      Gen
        .oneOf(true, false)
        .flatMap(named =>
          `object`(
            named = named,
            indentationStep = indentationStep,
            recDepthMax = recDepthMax
          )
        ),
      sep = wspBetweenObjs(0, indentationStep = indentationStep)
    )
  } yield (metas :: objs :: Nil).mkString

}
