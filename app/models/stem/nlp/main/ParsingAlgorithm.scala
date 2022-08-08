package models.stem.nlp.main

import models.stem.nlp.ParsingError
import models.stem.nlp.datastructures.syntax._

trait ParsingAlgorithm {
  def parseSigns(Signs: Set[Sign]): Either[ParsingError, Set[Sign]]
}



