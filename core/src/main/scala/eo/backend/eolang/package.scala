package eo.backend

// TODO: cover with tests
package object eolang {
  type InlineOrLines = Either[String, Iterable[String]]

  type Inline = Left[String, Iterable[String]]
  val Inline: String => Left[String, Iterable[String]] = Left[String, Iterable[String]]

  type Lines = Right[String, Iterable[String]]
  val Lines: Iterable[String] => Right[String, Iterable[String]] = Right[String, Iterable[String]]
}
