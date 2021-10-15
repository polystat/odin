package org.polystat.odin.parser

import org.scalacheck.Gen

object Gens {

  val wsp: Gen[String] =
    between(1, 3, Gen.oneOf(' ', '\t'))
      .map(_.mkString)

  val optWsp: Gen[String] = between(0, 2, wsp).map(_.mkString)
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
      comment <- between(0, 15, Gen.alphaLowerChar).map(_.mkString)
      eol <- eol
    } yield s"$before#$comment$eol"
    between(0, 3, Gen.oneOf(emptyLine, comment)).map(_.mkString)
  }

  def between[T](min: Int, max: Int, gen: Gen[T]): Gen[List[T]] = {
    for {
      len <- Gen.choose(min, max)
      gens <- Gen.listOfN(len, gen)
      lst <- gens
    } yield lst
  }

  def surroundedBy[T, S](gen: Gen[T], sur: Gen[S]): Gen[String] = for {
    before <- sur
    thing <- gen
    after <- sur
  } yield s"$before$thing$after"

  val digit: Gen[Char] = Gen.numChar

  val digits: Gen[String] =
    between(1, 5, digit)
      .map(_.mkString)

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
  } yield (fst :: rest).mkString

  val packageName: Gen[String] =
    between(1, 3, identifier)
      .map(_.mkString("."))

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
    version <- between(3, 3, digits.suchThat(s => !s.startsWith("0")))
      .map(_.mkString("."))
  } yield s"$pkgName:$artifactName:$version"

  val rtMeta: Gen[String] = for {
    alias <- surroundedBy(identifier, wsp)
    artifactId <- artifactName
  } yield s"+rt$alias$artifactId"

  val metas: Gen[String] = for {
    header <- emptyLinesOrComments
    pkg <- between(0, 1, packageMeta).map(_.mkString)
    pkgEol <- eol
    metas <- between(
      0,
      5,
      for {
        comments <- emptyLinesOrComments
        meta <- Gen.oneOf(rtMeta, aliasMeta)
        metaEol <- eol
      } yield s"$comments$meta$metaEol"
    ).map(_.map(_.mkString).mkString)
  } yield s"$header$pkg$pkgEol$metas"

}
