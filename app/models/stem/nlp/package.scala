package models.stem

package object nlp {
  type Conj = Set[Map[String, Set[String]]]

  case class ParsingError(msg: String)
}
