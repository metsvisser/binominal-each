package models.stem.nlp.main

import models.stem.nlp.{Conj, ParsingError}
import models.stem.nlp.datastructures.semantics.{Entity, ModelEvent, Pred, Sem, SentenceEvent}
import models.stem.nlp.datastructures.syntax.{Lexeme, Sign, Syn, Word}

class Parser(language: Language) {

  val shiftReduceParser = new ShiftReduceParser

  def parseSentence(input: String,
                    model: Set[Sem]
                   ): Either[ParsingError, Option[Conj]] = {
    val sentenceAsListOfStrings: Array[String] = input.split(" ")
    val modelLexemes = extractFromModel(model, language.vocabulary)
    for {
      denotatedSigns <- convertToSigns(sentenceAsListOfStrings, modelLexemes)
      parsedSigns <- shiftReduceParser.parseSigns(denotatedSigns)
      sentenceEvent <- filterSentenceDenotations(parsedSigns, sentenceAsListOfStrings.size)
      truth <- computeTruth(sentenceEvent, model)
    } yield {
      truth
    }
  }

  def wordToSign(word: Word, vocab: Set[Lexeme], meaningVocab: Set[Sem]): Set[Sign] = {
    // kijken of lexemes en meanings kunnen worden samengevoegd
    val lexemes = vocab.collect({ case l if l.word == word.word => l })
    for {
      l <- lexemes
      m <- meaningVocab
      if l.base == m.name
    } yield {
      Sign(Syn(word, l.pos, l.features, l.args), m)
    }
  }

  def convertToSigns(sentenceAsListOfStrings: Array[String], modelLexemes: (Set[Lexeme], Set[Sem])): Either[ParsingError, Set[Sign]] = {
    val sentenceAsSetOfWords = sentenceAsListOfStrings.indices.map(i => Word(sentenceAsListOfStrings(i), i, i + 1)).toSet
    val vocab = language.vocabulary.items ++ modelLexemes._1
    val meaningVocab = language.vocabulary.fixedDenotations ++ modelLexemes._2

    val signs = sentenceAsSetOfWords.flatMap { word =>
      wordToSign(word, vocab, meaningVocab)
    }
    Right(signs)
  }


  def extractFromModel(model: Set[Sem], vocabulary: Vocabulary): (Set[Lexeme], Set[Sem]) = {
    // neem de entities en voeg die toe aan de SYN en SEM vocabs
    val entities: Set[Sem] = model.collect { case e: Entity => e }
    val namedEntities = entities
      .map(e => Lexeme(e.name, e.name, "np"))
      .map(e => e.addFeatures(vocabulary.lexicalRules))
    // neem de preds en voeg die toe aan de SYN en SEM vocabs (staan toch al in de SYN vocab?)
    val preds: Set[Sem] = model.collect { case e: Pred => e }
    val nouns = preds
      .map(e => Lexeme(e.name, e.name, "n"))
      .map(e => e.addFeatures(vocabulary.lexicalRules))
    val lexemes = namedEntities ++ nouns
    // neem de events en voeg die toe aan de SEM vocab (staat al in de SYN vocab)
    val events: Set[Sem] = model.collect { case e: ModelEvent => e }
      .map(modelEvent => SentenceEvent(modelEvent.name, Set(Set(Map()))))
    val denotations: Set[Sem] = entities ++ preds ++ events
    (lexemes, denotations)
  }

  def filterSentenceDenotations(parsedSigns: Set[Sign], sentenceLength: Int): Either[ParsingError, SentenceEvent] = {
    for {
      // This will never be a left, because the option will never be None
      correctLength <- Option(parsedSigns
        .filter(_.syn.getNrOfWords() == sentenceLength))
        .toRight(ParsingError("No Sign that spans the entire input sentence"))
      event <- correctLength
        .map(_.sem)
        // only keep the first sentence Event found
        .collectFirst { case e: SentenceEvent => e }
        .toRight(ParsingError("No event spanning the entire input sentence"))
    } yield {
      event
    }
  }

  def computeTruth(sentenceEvent: SentenceEvent,
                   model: Set[Sem])
  : Either[ParsingError, Option[Conj]] = {
    model
      .filter(e => e.name == sentenceEvent.name)
      .collectFirst { case e: ModelEvent => e }.toRight(ParsingError("No event found inside the model"))
      .map { modelEvent =>
        // a sentence is true if there is a CONJ in its DISJ
        // that is a subset of the CONJ in the model event
        val truth = sentenceEvent.eventDisjunction.find(conj => conj subsetOf modelEvent.truth)
        truth
      }
  }

}
