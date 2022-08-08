package models.stem.nlp.datastructures.syntax

case class Feature(feature: String, value: String) {

  override def toString: String = "[" + feature + " = " + value + "]"
}
