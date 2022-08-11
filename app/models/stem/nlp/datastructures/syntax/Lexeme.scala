package models.stem.nlp.datastructures.syntax

case class Lexeme(word: String,
                  base: String,
                  pos: String,
                  features: Set[Feature] = Set.empty[Feature],
                  args: Set[Argument] = Set.empty[Argument]) {

  def addFeatures(lexicalRules: Set[LexicalRule]): Lexeme = {
    val featuresNew = lexicalRules.find(_.pos == pos).fold(features)(r => features.union(r.features))
    Lexeme(word, base, pos, featuresNew, args)
  }

  def addArguments(grammarRules: Set[GrammarRule]): Lexeme = {
    val args = grammarRules.collect{ case rule if rule.functorpos == pos => rule }.map { rule =>
      Argument(rule.functorpos, rule.argpos, rule.resultpos, rule.unify, rule.position)
    }
    Lexeme(word, base, pos, features, args)
  }

}
