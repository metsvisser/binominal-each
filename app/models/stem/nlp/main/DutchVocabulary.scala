package models.stem.nlp.main

import models.stem.nlp.datastructures.semantics.{Filter, ModelEvent, Quant, Sem, SentenceEvent}
import models.stem.nlp.datastructures.syntax.{Argument, Feature, GrammarRule, Lexeme}
import models.stem.nlp.stem.nlp.utils.Utilities

import scala.io.Source

class DutchVocabulary extends Vocabulary {

  override val items: Set[Lexeme] = loadItems
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

  private def loadItems: Set[Lexeme] = {
    loadClosedClassItems ++ loadOpenClassItems
  }

  private def loadClosedClassItems: Set[Lexeme] = {
    val vocabItems = for (line <- Source.fromFile("app/files/closedclass.txt").getLines().toList) yield {
      val ar: Array[String] = line.split('|')
      val word: String = ar(0)
      val pos: String = ar(1)
      val featureString: String = ar(3)
      val features: Set[Feature] = Utilities.processFeatures(featureString, this.features)
      val item: Lexeme = Lexeme(word, word, pos, features)
      addFeatures(item)
      addArguments(item)
    }
    vocabItems.toSet
  }

  private def loadOpenClassItems: Set[Lexeme] = {
    loadNouns ++ loadAdjs ++ loadVerbs
  }

  private def loadNouns: Set[Lexeme] = {
    val nouns = for (line <- Source.fromFile("app/files/nouns.txt").getLines().toList) yield {
      val vocab: Array[String] = line.split('|')
      val cat: String = "n"
      val sgword: String = vocab(0)
      val plword: String = vocab(1)
      val geslacht: String = vocab(3)
      val sgLexeme: Lexeme = Lexeme(sgword, sgword, cat, Set(getF("sg"), getF(geslacht)).flatten)
      val plLexeme: Lexeme = Lexeme(plword, sgword, cat, Set(getF("pl"), getF(geslacht)).flatten)
      List(sgLexeme, plLexeme).map(addFeatures)
    }
    nouns.flatten.toSet
  }

  private def loadAdjs: Set[Lexeme] = {
    val adjs = for (line <- Source.fromFile("app/files/adjectives.txt").getLines().toList) yield {
      val vocab: Array[String] = line.split('|')
      val cat: String = "a"
      val zonder: String = vocab(0)
      val zonderLexeme: String = vocab(1)
      val metLexeme: String = vocab(2)
      val zon: Lexeme = Lexeme(zonderLexeme, zonder, cat, Set(getF("indef"), getF("neuter"), getF("sg")).flatten)
      val met1: Lexeme = Lexeme(metLexeme, zonder, cat, Set(getF("def"), getF("neuter"), getF("sg")).flatten)
      val met2: Lexeme = Lexeme(metLexeme, zonder, cat, Set(getF("male"), getF("sg")).flatten)
      val met3: Lexeme = Lexeme(metLexeme, zonder, cat, Set(getF("pl")).flatten)
      List(zon, met1, met2, met3).map(addFeatures)
    }
    adjs.flatten.toSet
  }

  private def loadVerbs: Set[Lexeme] = {
    val verbs = for (line <- Source.fromFile("app/files/verbs.txt").getLines().toList) yield {
      val vocab: Array[String] = line.split('|')
      val cat: String = "v"
      val wij: String = vocab(0)
      val hij: String = vocab(2)
      val roleString: String = vocab(6)
      val rolesArray = roleString.split(';')
      val roles: Set[Argument] = (for (role <- rolesArray) yield {
        val fields = role.split(',')
        val pos = fields(0)
        val name = fields(1)
        val unify = fields(2)
        val position = fields(3)
        val distribution = fields(4)
        Argument("v", pos, "v", unify, position, distribution, name)
      }).toSet
      val hijLexeme: Lexeme = Lexeme(hij, wij, cat, Set(getF("3rd"), getF("sg"), getF("ind")).flatten, roles)
      val wijLexeme: Lexeme = Lexeme(wij, wij, cat, Set(getF("pl"), getF("ind")).flatten, roles)
      List(hijLexeme, wijLexeme).map(addFeatures)
    }
    verbs.flatten.toSet
  }

  override def loadGrammarRules: Set[GrammarRule] = {
    val grammarRules = for (line <- Source.fromFile("app/files/grammarRules.txt").getLines().toList) yield {

      val ar: Array[String] = line.split('|')
      val f: String = ar(0)
      val a: String = ar(1)
      val r: String = ar(2)
      val unify: String = ar(3)
      val position: String = ar(4)
      val rule: GrammarRule = GrammarRule(f, a, r, unify, position)
      rule
    }
    grammarRules.toSet
  }

}
