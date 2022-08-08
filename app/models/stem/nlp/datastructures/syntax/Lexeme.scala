package models.stem.nlp.datastructures.syntax

case class Lexeme(word: String,
                  base: String,
                  pos: String,
                  features: Set[Feature] = Set.empty[Feature],
                  args: Set[Argument] = Set.empty[Argument]) {

}
