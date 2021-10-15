package org.polystat.odin.parser

import org.scalacheck.Gen

object Gens {

  val wsp: Gen[String] =
    between(1, 3, Gen.oneOf(' ', '\t'))
      .map(_.mkString)

  val optWsp: Gen[String] = between(0, 2, wsp)
  val eol: Gen[String] = Gen.oneOf("\n", "\r\n")

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

  def between[T](
    min: Int,
    max: Int,
    gen: Gen[T],
    sep: String = ""
  ): Gen[String] = {
    for {
      len <- Gen.choose(min, max)
      gens <- Gen.listOfN(len, gen)
    } yield gens.mkString(sep)
  }

  def surroundedBy[T, S](gen: Gen[T], sur: Gen[S]): Gen[String] = for {
    before <- sur
    thing <- gen
    after <- sur
  } yield s"$before$thing$after"

  val digit: Gen[Char] = Gen.numChar

  val digits: Gen[String] =
    between(1, 5, digit)

  val integer: Gen[String] = for {
    sign <- Gen.frequency(
      (10, ""),
      (10, "-"),
      (1, "+")
    )
    num <- digits
  } yield sign + num

  val identifierChar: Gen[Char] =
    Gen.frequency[Char](
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
      between(3, 3, digits.suchThat(s => !s.startsWith("0")), sep = ".")
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
    ).map(_.mkString)
  } yield (header :: pkg :: pkgEol :: metas :: Nil).mkString

  val idOrPhi: Gen[String] = Gen.frequency(
    (9, identifier),
    (1, "@")
  )

  val bndName: Gen[String] = for {
    op <- surroundedBy(">", optWsp)
    id <- idOrPhi
    before <- optWsp
    exclamationMark <- between(0, 1, "!")
    after <- optWsp
  } yield (op :: id :: before :: exclamationMark :: after :: Nil).mkString

  val abstractionParams: Gen[String] = for {
    params <- between(0, 4, idOrPhi, " ")
    vararg <- between(0, 1, if (params.nonEmpty) "..." else "")
  } yield s"[$params$vararg]"

  val attributeName: Gen[String] = Gen.frequency(
    (1, "@"),
    (1, "$"),
    (1, "^"),
    (10, identifier)
  )

}
