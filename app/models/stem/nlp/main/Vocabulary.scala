package models.stem.nlp.main

import models.stem.nlp.datastructures.semantics.Sem
import models.stem.nlp.datastructures.syntax._
import models.stem.nlp.stem.nlp.utils.Utilities

import scala.io.Source

trait Vocabulary {

  val features: Set[Feature] = loadFeatures()
  println("Features loaded: " + features.size)
  val lexicalRules: Set[LexicalRule] = loadRules
  println("Rules loaded: " + lexicalRules.size)
  val grammarRules: Set[GrammarRule] = loadGrammarRules

  def loadGrammarRules: Set[GrammarRule]



  // open class items
  val items: Set[Lexeme]
  // closed class items
  val fixedDenotations: Set[Sem]

  private def loadFeatures(): Set[Feature] = {
    val features = for (line <- Source.fromFile("app/files/dutch/features.txt").getLines().toList) yield {
      val ar: Array[String] = line.split(" ")
      val featureString: String = ar(0)
      val value: String = ar(1)
      val feature: Feature = Feature(featureString, value)
      feature
    }
    features.toSet
  }

  def splitFeatures(featureSplit: Array[String]): Set[Feature] = {
    if (featureSplit(0).trim().equals("x")) return Set.empty
    val features: Set[Feature] = (for (feature <- featureSplit) yield {
      Feature(feature.split("=")(0), feature.split("=")(1))
    }).toSet
    features.filter(f => Utilities.matchFeatures(f, features))
  }

  private def loadRules: Set[LexicalRule] = {
    val rules = for (line <- Source.fromFile("app/files/lexicalRules.txt").getLines().toList) yield {
      val ar: Array[String] = line.split('|')
      val pos: String = ar(0)
      val featureString: String = ar(2)
      val featureSplit: Array[String] = featureString.split(";")
      val featuresToBeAdded: Set[Feature] = splitFeatures(featureSplit)
      val rule: LexicalRule = LexicalRule(pos, featuresToBeAdded)
      rule
    }
    rules.toSet
  }

  def addFeatures(l: Lexeme): Lexeme = {
    val ruleOpt = lexicalRules.find(p => p.pos == l.pos)
    ruleOpt.map { rule =>
      Lexeme(l.word, l.base, l.pos, l.features.union(rule.features), l.args)
    }
  }.getOrElse(l)

  def addArguments(l: Lexeme): Lexeme = {
    val rules = grammarRules.collect{ case rule if rule.functorpos == l.pos => rule }
    val args = rules.map { rule =>
      Argument(rule.functorpos, rule.argpos, rule.resultpos, rule.unify, rule.position)
    }
    Lexeme(l.word, l.base, l.pos, l.features, args)
  }
}