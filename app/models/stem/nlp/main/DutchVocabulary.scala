package models.stem.nlp.main

import models.stem.nlp.datastructures.semantics.{Filter, Quant, Sem}
import models.stem.nlp.datastructures.syntax.{Argument, Feature, GrammarRule, Lexeme}
import models.stem.nlp.stem.nlp.utils.Utilities

import scala.io.Source

class DutchVocabulary extends Vocabulary {

  override val items: Set[Lexeme] = loadClosedClassItems ++ loadNouns ++ loadAdjs ++ loadVerbs
  override val fixedDenotations: Set[Sem] = loadFixedMeanings

  def loadFixedMeanings: Set[Sem] = {
    val alle = Quant("alle", Quant.alle)
    val een = Quant("een", Quant.een)
    val drie = Quant("drie", Quant.drie)
    val twee = Quant("twee", Quant.twee)
    val dezelfde = Filter("dezelfde", Filter.dezelfde)
    val verschillende = Filter("verschillende", Filter.verschillende)
    Set(alle, drie, een, twee, dezelfde, verschillende)
  }

  private def getF(value: String): Option[Feature] =
    features.find(value == _.value)

  private def loadClosedClassItems: Set[Lexeme] = {
    val bufferedSource = scala.io.Source.fromFile("app/files/closedclass.txt")
    val vocabItems = bufferedSource.getLines().map { line =>
      val ar: Array[String] = line.split('|')
      Lexeme(ar(0), ar(0), ar(1), Utilities.processFeatures(ar(3)).intersect(features))
        .addFeatures(lexicalRules)
        .addArguments(grammarRules)
    }
    val vocabItemSet = vocabItems.toSet
    bufferedSource.close()
    vocabItemSet
  }

  private def loadNouns: Set[Lexeme] = {
    val bufferedSource = scala.io.Source.fromFile("app/files/nouns.txt")
    val nouns = bufferedSource.getLines().flatMap { line =>
      val vocab: Array[String] = line.split('|')
      val sgLexeme: Lexeme = Lexeme(vocab(0), vocab(0), "n", Set(getF("sg"), getF(vocab(3))).flatten)
      val plLexeme: Lexeme = Lexeme(vocab(1), vocab(0), "n", Set(getF("pl"), getF(vocab(3))).flatten)
      List(sgLexeme, plLexeme).map(_.addFeatures(lexicalRules))
    }
    val nounSet = nouns.toSet
    bufferedSource.close()
    nounSet
  }

  private def loadAdjs: Set[Lexeme] = {
    val bufferedSource = scala.io.Source.fromFile("app/files/adjectives.txt")
    val adjs = bufferedSource.getLines().flatMap { line =>
      val vocab: Array[String] = line.split('|')
      val zon: Lexeme = Lexeme(vocab(1), vocab(0), "a", Set(getF("indef"), getF("neuter"), getF("sg")).flatten)
      val met1: Lexeme = Lexeme(vocab(2), vocab(0), "a", Set(getF("def"), getF("neuter"), getF("sg")).flatten)
      val met2: Lexeme = Lexeme(vocab(2), vocab(0), "a", Set(getF("male"), getF("sg")).flatten)
      val met3: Lexeme = Lexeme(vocab(2), vocab(0), "a", Set(getF("pl")).flatten)
      List(zon, met1, met2, met3).map(_.addFeatures(lexicalRules))
    }
    val adjSet = adjs.toSet
    bufferedSource.close()
    adjSet
  }

  private def loadVerbs: Set[Lexeme] = {
    val bufferedSource = scala.io.Source.fromFile("app/files/verbs.txt")
    val verbs = bufferedSource.getLines().flatMap { line =>
      val vocab: Array[String] = line.split('|')
      val rolesArray = vocab(6).split(';')
      val roles: Set[Argument] = (for (role <- rolesArray) yield {
        val fields = role.split(',')
        Argument("v", fields(0), "v", fields(2), fields(3), fields(4), fields(1))
      }).toSet
      val hijLexeme: Lexeme = Lexeme(vocab(2), vocab(0), "v", Set(getF("3rd"), getF("sg"), getF("ind")).flatten, roles)
      val wijLexeme: Lexeme = Lexeme(vocab(0), vocab(0), "v", Set(getF("pl"), getF("ind")).flatten, roles)
      List(hijLexeme, wijLexeme).map(_.addFeatures(lexicalRules))
    }
    val verbSet = verbs.toSet
    bufferedSource.close()
    verbSet
  }

  override def loadGrammarRules: Set[GrammarRule] = {
    val bufferedSource = scala.io.Source.fromFile("app/files/grammarRules.txt")
    val grammarRules = bufferedSource.getLines().map { line =>
      val ar: Array[String] = line.split('|')
      GrammarRule(ar(0), ar(1), ar(2), ar(3), ar(4))
    }
    val grammerRulesSet = grammarRules.toSet
    bufferedSource.close()
    grammerRulesSet
  }
}
