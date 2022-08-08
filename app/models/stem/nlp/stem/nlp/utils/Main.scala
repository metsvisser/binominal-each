package models.stem.nlp.stem.nlp.utils

import models.stem.nlp.datastructures.models.ModelParser
import models.stem.nlp.datastructures.syntax.Sign
import models.stem.nlp.main.{DutchLanguage, Parser, ShiftReduceParser}

object Main {
  def main(args: Array[String]): Unit = {

    val modelParser = new ModelParser
    val model2 = modelParser.convertToModel(Set.empty, Set("kat"))

    val dutchLanguage = new DutchLanguage
    val parser = new Parser(dutchLanguage)
    val shiftReduceParser = new ShiftReduceParser
    val model2Lexemes = parser.extractFromModel(model2, dutchLanguage.vocabulary)


    val sentenceAsListOfStrings: Array[String] = "een kat".split(" ")

    val either = for {
      denotatedSigns <- parser.convertToSigns(sentenceAsListOfStrings, model2Lexemes)
      parsedSigns <- shiftReduceParser.parseSigns(denotatedSigns)
    } yield parsedSigns
    val denotations = either.getOrElse(Set.empty[Sign])

    denotations.foreach(println(_))
  }
}