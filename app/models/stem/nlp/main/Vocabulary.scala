package models.stem.nlp.main

import models.stem.nlp.datastructures.semantics.Sem
import models.stem.nlp.datastructures.syntax._
import models.stem.nlp.stem.nlp.utils.Utilities

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
    val bufferedSource = scala.io.Source.fromFile("app/files/features.txt")
    val features = bufferedSource.getLines().map { line =>
      val ar: Array[String] = line.split(" ")
      Feature(ar(0), ar(1))
    }
    val featureSet = features.toSet
    bufferedSource.close()
    featureSet
  }

  def splitFeatures(featureSplit: Array[String]): Set[Feature] = {
    if (featureSplit(0).trim().equals("x")) return Set.empty
    val itemFeatures: Set[Feature] = (for (feature <- featureSplit) yield {
      Feature(feature.split("=")(0), feature.split("=")(1))
    }).toSet
    itemFeatures.intersect(features)
  }

  private def loadRules: Set[LexicalRule] = {
    val bufferedSource = scala.io.Source.fromFile("app/files/lexicalRules.txt")
    val rules = bufferedSource.getLines().map { line =>
      val ar: Array[String] = line.split('|')
      val featuresToBeAdded: Set[Feature] = splitFeatures(ar(2).split(";"))
      LexicalRule(ar(0), featuresToBeAdded)
    }
    val ruleSet = rules.toSet
    bufferedSource.close()
    ruleSet
  }

}