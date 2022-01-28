package org.polystat.odin.parser

import org.scalacheck.Gen

package object gens {

  def betweenStr[T](
    min: Int,
    max: Int,
    gen: Gen[T],
    sep: Gen[String] = ""
  ): Gen[String] = {
    sep.flatMap(sep => between(min, max, gen).map(_.mkString(sep)))
  }

  def between[T](
    min: Int,
    max: Int,
    gen: Gen[T],
  ): Gen[List[T]] = {
    for {
      len <- Gen.choose(min, max)
      gens <- Gen.listOfN(len, gen)
    } yield gens
  }

  def surroundedBy[T, S](gen: Gen[T], sur: Gen[S]): Gen[String] = for {
    before <- sur
    thing <- gen
    after <- sur
  } yield s"$before$thing$after"

}
