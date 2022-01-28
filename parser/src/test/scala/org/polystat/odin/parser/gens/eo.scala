package org.polystat.odin.parser.gens

import org.scalacheck.Gen

object eo {

  val wsp: Gen[String] =
    betweenStr(
      1,
      2,
      Gen.frequency(
        (1, "\t"),
        (19, " ")
      )
    ).map(_.mkString)

  val optWsp: Gen[String] = betweenStr(
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
      comment <- betweenStr(0, 15, Gen.alphaLowerChar)
      eol <- eol
    } yield s"$before#$comment$eol"
    betweenStr(0, 2, Gen.oneOf(emptyLine, comment))
  }

  val digit: Gen[Char] = Gen.numChar
  val digits: Gen[String] = betweenStr(1, 5, digit)
  val nonZeroDigit: Gen[Char] = Gen.oneOf('1' to '9')

  val nonZeroInteger: Gen[String] = for {
    sign <- Gen.oneOf("", "-")
    first <- nonZeroDigit
    rest <- betweenStr(0, 1, digits)
  } yield (sign :: first :: rest :: Nil).mkString

  val integer: Gen[String] = Gen.frequency(
    (1, "0"),
    (99, nonZeroInteger)
  )

  val float: Gen[String] = for {
    before <- integer
    after <- digits
  } yield s"$before.$after"

  val escapedUnicode: Gen[String] = betweenStr(4, 4, digit).map("\\u" + _)

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

  val undelimitedString: Gen[String] =
    betweenStr(
      0,
      15,
      undelimitedChar
    )

  val string: Gen[String] =
    undelimitedString.map(str => s"\"$str\"")

  val char: Gen[String] = undelimitedChar
    .retryUntil(_ != "'")
    .map(c => s"'$c'")

  val identifierChar: Gen[Char] =
    Gen.frequency(
      (15, smallLetter),
      (15, letter),
      (8, Gen.numChar),
      (1, '_'),
      (1, '-'),
    )

  val identifier: Gen[String] = for {
    fst <- smallLetter
    rest <- betweenStr(0, 3, identifierChar)
  } yield fst +: rest

  val packageName: Gen[String] =
    betweenStr(1, 3, identifier, sep = ".")

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
      betweenStr(3, 3, digits.retryUntil(s => !s.startsWith("0")), sep = ".")
  } yield s"$pkgName:$artifactName:$version"

  val rtMeta: Gen[String] = for {
    alias <- surroundedBy(identifier, wsp)
    artifactId <- artifactName
  } yield s"+rt$alias$artifactId"

  val metas: Gen[String] = for {
    header <- emptyLinesOrComments
    pkg <- betweenStr(0, 1, packageMeta)
    pkgEol <- eol
    metas <- betweenStr(
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
    exclamationMark <- surroundedBy(betweenStr(0, 1, "!"), optWsp)
  } yield (op :: id :: exclamationMark :: Nil).mkString

  val abstractionParams: Gen[String] = for {
    params <- betweenStr(0, 5, paramName, sep = wsp)
    vararg <- betweenStr(0, 1, if (params.nonEmpty) "..." else "")
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
    maxDepth: Int,
    depth: Int = 0,
  ): Gen[String] = {

    val simpleApplicationTarget = Gen.oneOf(
      data,
      attributeName
    )

    def parenthesized(maxDepth: Int, depth: Int): Gen[String] =
      if (depth < maxDepth)
        singleLineApplication(
          depth = depth,
          maxDepth = maxDepth
        )
          .map(s => s"($s)")
      else
        simpleApplicationTarget

    def singleLineEoBnd(maxDepth: Int, depth: Int): Gen[String] = for {
      expr <- singleLineApplication(maxDepth, depth)
      bndName <- eo.bndName
    } yield List("(", expr, bndName, ")").mkString

    def singleLineAbstraction(maxDepth: Int, depth: Int): Gen[String] = for {
      params <- eo.abstractionParams
      args <- between(0, 4, singleLineEoBnd(maxDepth, depth + 1))
      wsp <-  wsp
    } yield {
      if (args.isEmpty)
        List[String]("(", params, ")").mkString
      else
        List[String](params, wsp, args.mkString(" ")).mkString
    }

    def attributeChain(maxDepth: Int, depth: Int): Gen[String] = for {
      trg <-
        Gen.oneOf(simpleApplicationTarget, parenthesized(maxDepth, depth + 1))
      attrs <- betweenStr(1, 3, attributeName, sep = ".")
    } yield s"$trg.$attrs"

    def applicationTarget(maxDepth: Int, depth: Int): Gen[String] = Gen.oneOf(
      parenthesized(maxDepth, depth),
      simpleApplicationTarget,
      attributeChain(maxDepth, depth)
    )

    def horizontalApplicationArgs(maxDepth: Int, depth: Int): Gen[String] = {
      val arg =
        if (depth < maxDepth)
          applicationTarget(maxDepth, depth + 1)
        else
          simpleApplicationTarget
      betweenStr(1, 5, arg, sep = wsp)
    }

    def justApplication(maxDepth: Int, depth: Int): Gen[String] = for {
      trg <-
        if (depth < maxDepth)
          applicationTarget(maxDepth, depth + 1)
        else
          simpleApplicationTarget
      sep <- wsp
      args <- horizontalApplicationArgs(maxDepth, depth + 1)
    } yield (trg :: sep :: args :: Nil).mkString

    def singleLineArray(maxDepth: Int, depth: Int): Gen[String] = for {
      sep <- optWsp
      elem =
        if (depth < maxDepth)
          Gen.oneOf(
            parenthesized(maxDepth, depth + 1),
            applicationTarget(maxDepth, depth + 1)
          )
        else simpleApplicationTarget
      elems <- betweenStr(0, 4, elem, sep = wsp)
    } yield s"*$sep$elems"

    if (depth < maxDepth)
      Gen.oneOf(
        justApplication(maxDepth, depth),
        applicationTarget(maxDepth, depth + 1),
        singleLineArray(maxDepth, depth),
        singleLineAbstraction(maxDepth, depth),
      )
    else
      simpleApplicationTarget
  }

  val nothing: Gen[String] = Gen.const("")

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
    maxDepth: Int,
    recDepth: Int = 0,
    includeInverseDot: Boolean = true,
  ): Gen[String] = {
    val abstraction = for {
      params <- abstractionParams
      name <- if (named) bndName else nothing
      ifAttrs <- Gen.oneOf(true, false)
      attrs <-
        if (recDepth < maxDepth && ifAttrs)
          boundAttributes(
            recDepth = recDepth + 1,
            indentationStep = indentationStep,
            maxDepth = maxDepth,
          )
        else nothing
    } yield (params :: name :: attrs :: Nil).mkString

    val inverseDotApplication = for {
      id <- identifier
      name <- if (named) bndName else nothing
      attrs <- verticalApplicationArgs(
        recDepth = recDepth + 1,
        indentationStep = indentationStep,
        maxDepth = maxDepth,
        includeInverseDot = recDepth < maxDepth
      )
    } yield (id :: "." :: name :: attrs :: Nil).mkString

    val regularApplication = for {
      trg <- singleLineApplication(maxDepth = maxDepth)
      name <- if (named) bndName else nothing
      ifArgs <- Gen.oneOf(true, false)
      args <-
        if (recDepth < maxDepth && ifArgs)
          verticalApplicationArgs(
            recDepth = recDepth + 1,
            indentationStep = indentationStep,
            maxDepth = maxDepth,
          )
        else nothing
    } yield (trg :: name :: args :: Nil).mkString

    val verticalArray = for {
      name <- if (named) bndName else nothing
      ifHasItems <- Gen.oneOf(true, false)
      items <-
        if (recDepth < maxDepth && ifHasItems)
          verticalApplicationArgs(
            recDepth = recDepth + 1,
            indentationStep = indentationStep,
            maxDepth = maxDepth,
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
    maxDepth: Int,
    includeInverseDot: Boolean = true,
  ): Gen[String] = for {
    before <- wspBetweenObjs(recDepth, indentationStep)
    named <- Gen.oneOf(true, false)
    objs <- betweenStr(
      1,
      3,
      `object`(
        recDepth = recDepth,
        named = named,
        indentationStep = indentationStep,
        maxDepth = maxDepth,
        includeInverseDot = includeInverseDot
      ),
      sep = wspBetweenObjs(recDepth, indentationStep)
    )
  } yield (before :: objs :: Nil).mkString

  def boundAttributes(
    recDepth: Int,
    indentationStep: Int,
    maxDepth: Int,
  ): Gen[String] = for {
    before <- wspBetweenObjs(recDepth, indentationStep)
    objs <- betweenStr(
      1,
      3,
      `object`(
        recDepth = recDepth,
        named = true,
        indentationStep = indentationStep,
        maxDepth = maxDepth
      ),
      sep = wspBetweenObjs(recDepth, indentationStep)
    )
  } yield (before :: objs :: Nil).mkString

  def program(
    indentationStep: Int,
    maxDepth: Int,
  ): Gen[String] = for {
    metas <- metas
    objs <- betweenStr(
      0,
      4,
      Gen
        .oneOf(true, false)
        .flatMap(named =>
          `object`(
            named = named,
            indentationStep = indentationStep,
            maxDepth = maxDepth
          )
        ),
      sep = wspBetweenObjs(0, indentationStep = indentationStep)
    )
  } yield (metas :: objs :: Nil).mkString
}
