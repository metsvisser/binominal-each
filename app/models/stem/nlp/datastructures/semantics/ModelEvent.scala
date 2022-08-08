package models.stem.nlp.datastructures.semantics

case class ModelEvent(name: String, truth: Set[Map[String, Set[String]]]) extends Sem
