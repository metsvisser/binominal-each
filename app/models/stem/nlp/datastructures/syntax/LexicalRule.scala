package models.stem.nlp.datastructures.syntax

case class LexicalRule(pos: String, features: Set[Feature]) {

  override def toString: String = {
    var fea: String = ""
    for (feature <- features) {
      fea = fea + feature.toString + ","
    }
    "PoS: " + pos + " POS:  " + pos + " Features: " + fea
  }

}