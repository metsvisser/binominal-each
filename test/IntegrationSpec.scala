import org.scalatestplus.play._
import models.frontend.Person
import models.stem.nlp.datastructures.models.ModelParser
import models.stem.nlp.datastructures.semantics.{Entity, Pred, Quant, Sem, SentenceEvent}
import models.stem.nlp.datastructures.syntax.{Argument, Lexeme, Sign, Syn, Word}
import models.stem.nlp.main.{DutchLanguage, Parser, ShiftReduceParser}

import scala.language.postfixOps

class IntegrationSpec extends PlaySpec {

  val input = "jan heeft een kat"

  val jan = Word("jan", 0, 1)
  val heeft = Word("heeft", 1, 2)
  val een = Word("een", 2, 3)
  val kat = Word("kat", 3, 4)

  val recognizedWords = List(
    jan, heeft, een, kat
  )

  val pets = Set("hond", "kat", "spin")
  val jann = Person("jan", "male", false, Set("hond", "kat"))
  val piet = Person("piet", "male", true, Set("hond", "spin"))
  val klaas = Person("klaas", "female", false, Set("kat"))

  val modelParser = new ModelParser
  val model: Set[Sem] = modelParser.convertToModel(Set(jann, piet, klaas), pets)

  val model2 = modelParser.convertToModel(Set.empty, Set("kat"))

  val dutchLanguage = new DutchLanguage
  val parser = new Parser(dutchLanguage)
  val shiftReduceParser = new ShiftReduceParser
  val modelLexemes = parser.extractFromModel(model, dutchLanguage.vocabulary)
  val model2Lexemes = parser.extractFromModel(model2, dutchLanguage.vocabulary)




  // test die features recognizet
  // test die POS assignet
  // test die ROLES assignet
  // test unification

  "The parser" must {
    "add denotations to all words" in {
      val sentenceAsListOfStrings: Array[String] = input.split(" ")
      val modelLexemes = parser.extractFromModel(model, dutchLanguage.vocabulary)
      val either = for {
        recognizedSigns <- parser.convertToSigns(sentenceAsListOfStrings, modelLexemes)
      } yield recognizedSigns

      val denotations = either.getOrElse(Set.empty[Sign])

      denotations.size mustBe 4
      denotations.map(_.sem).collect { case e: Entity => e }.size mustBe 1
      denotations.map(_.sem).collect { case v: SentenceEvent => v }.size mustBe 1

      denotations.map(_.sem).collect { case q: Quant => q }.size mustBe 1
      denotations.map(_.sem).collect { case p: Pred => p }.size mustBe 1
    }
    "apply lexical rules" in {
      val lexeme = Lexeme("jan", "jan", "np")
      val enriched = lexeme.addFeatures(dutchLanguage.vocabulary.lexicalRules)

      enriched.features.size mustBe 2
    }
    "apply grammar rules" in {
      val lexeme = Lexeme("een", "een", "d")
      val enriched = lexeme.addArguments(dutchLanguage.vocabulary.grammarRules)

      enriched.args.size mustBe 1
    }
    "convert a verb to a sign" in {
      val vocab = dutchLanguage.vocabulary.items ++ modelLexemes._1
      val meanings = dutchLanguage.vocabulary.fixedDenotations ++ modelLexemes._2
      val word = Word("heeft", 0, 1)
      val sign = parser.wordToSign(word,vocab , meanings)

      sign.size mustBe 1
    }
    "convert a detereminer to a sign and add arguments" in {
      val vocab = dutchLanguage.vocabulary.items ++ modelLexemes._1
      val meanings = dutchLanguage.vocabulary.fixedDenotations ++ modelLexemes._2
      val word = Word("een", 0, 1)
      val sign = parser.wordToSign(word,vocab , meanings)

      sign.size mustBe 1
      sign.head.syn.args.size mustBe 1
      sign.head.syn.args.head mustBe Argument("d","n","np", "unify", "ir")

    }
    "convert a named entity to a sign" in {
      val vocab = dutchLanguage.vocabulary.items ++ modelLexemes._1
      val meanings =  dutchLanguage.vocabulary.fixedDenotations ++ modelLexemes._2
      val word = Word("jan", 0, 1)
      val sign = parser.wordToSign(word, vocab, meanings)

      sign.size mustBe 1
    }
    "parse 'een kat'" in {
      val sentenceAsListOfStrings: Array[String] = "een kat".split(" ")

      val either = for {
        denotatedSigns <- parser.convertToSigns(sentenceAsListOfStrings, model2Lexemes)
        parsedSigns <- shiftReduceParser.parseSigns(denotatedSigns)
      } yield parsedSigns
      val denotations = either.getOrElse(Set.empty[Sign])

      // must contain the Entity corresponding to "een kat"
      denotations.map(_.sem).collect { case e: Entity => e }.size mustBe 1
    }
    "parse 'jan heeft'" in {
      val sentenceAsListOfStrings: Array[String] = "jan heeft".split(" ")

      val either = for {
        denotatedSigns <- parser.convertToSigns(sentenceAsListOfStrings, modelLexemes)
        parsedSigns <- shiftReduceParser.parseSigns(denotatedSigns)
      } yield parsedSigns
      val denotations = either.getOrElse(Set.empty[Sign])

      // must contain the Entity corresponding to "een kat"
      denotations.map(_.sem).collect { case e: Entity => e }.size mustBe 1
      denotations.map(_.sem).collect { case e: SentenceEvent => e }.size mustBe 2
    }
    "parse the Signs" in {
      val sentenceAsListOfStrings: Array[String] = input.split(" ")

      val either = for {
        denotatedSigns <- parser.convertToSigns(sentenceAsListOfStrings, modelLexemes)
        parsedSigns <- shiftReduceParser.parseSigns(denotatedSigns)
      } yield parsedSigns
      val denotations = either.getOrElse(Set.empty[Sign])

      denotations.size mustBe 8
      val setsofWords = denotations.map(_.syn.words)
      val comparisonSet = Set(
        Set(jan),
        Set(heeft),
        Set(een),
        Set(kat),
        Set(jan, heeft),
        Set(een, kat),
        Set(heeft, een, kat),
        Set(jan, heeft, een, kat)
      )
      setsofWords mustBe comparisonSet
      denotations.map(_.sem).collect { case e: Entity => e }.size mustBe 2
      denotations.map(_.sem).collect { case v: SentenceEvent => v }.size mustBe 4
      denotations.map(_.sem).collect { case q: Quant => q }.size mustBe 1
      denotations.map(_.sem).collect { case p: Pred => p }.size mustBe 1
    }
    "filter sentence denotations" in {
      val sentenceAsListOfStrings: Array[String] = input.split(" ")

      val either = for {
        denotatedSigns <- parser.convertToSigns(sentenceAsListOfStrings, modelLexemes)
        parsedSigns <- shiftReduceParser.parseSigns(denotatedSigns)
        sentenceEvent <- parser.filterSentenceDenotations(parsedSigns, sentenceAsListOfStrings.size)

      } yield sentenceEvent
      either.isRight mustBe true
    }
    "compute truth" in {
      val sentenceAsListOfStrings: Array[String] = input.split(" ")
      val either = for {
        denotatedSigns <- parser.convertToSigns(sentenceAsListOfStrings, modelLexemes)
        parsedSigns <- shiftReduceParser.parseSigns(denotatedSigns)
        sentenceEvent <- parser.filterSentenceDenotations(parsedSigns, sentenceAsListOfStrings.size)
        truth <- parser.computeTruth(sentenceEvent, model)
      } yield {
        truth
      }
      val value = either.getOrElse(Some(Set.empty[Map[String, Set[String]]]))
        value mustBe Some(Set(Map("bezitter" -> Set("jan"), "bezat" -> Set("kat"))))
    }
  }
}