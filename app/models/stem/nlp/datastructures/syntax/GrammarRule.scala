package models.stem.nlp.datastructures.syntax

case class GrammarRule(functorpos: String,
                       argpos: String,
                       resultpos: String,
                       unify: String,
                       position: String)  {

  override def toString: String = {
    functorpos + " + " + argpos + " --> " + resultpos + " unify: " +
      unify +
      " position: " +
      position
  }
}