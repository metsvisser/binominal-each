package models.stem.nlp.stem.nlp.utils

import models.stem.nlp.datastructures.syntax._
import models.stem.nlp.{Conj, ParsingError}


import scala.collection.mutable.LinkedHashSet

object Utilities {

  def matchFeatures(featureToBeAdded: Feature, featureSet: Set[Feature]): Boolean =
    featureSet.find(_ == featureToBeAdded).exists(_ => true)

  def processFeatures(featureString: String, featureSet: Set[Feature]): Set[Feature] = {
//    println("FEATURESTRING: " + featureString)
    val features: Array[String] = featureString.split(";")
    val featuresToBeAdded = LinkedHashSet[Feature]()
    for {
      feature <- features if feature.trim().!=("x")
    } {
//      println("FEATURE: "+feature)
      val splitFeature: Array[String] = feature.split("=")
      val featureString2: String = splitFeature(0)
      val value: String = splitFeature(1)
      val featureToBeAdded: Feature = new Feature(featureString2, value)
      if (!Utilities.matchFeatures(featureToBeAdded, featureSet)) {
        return Set.empty++featuresToBeAdded
      }
      featuresToBeAdded.add(featureToBeAdded)
    }
    Set.empty++featuresToBeAdded
  }

//  def convertToWords(input: String): Either[ParsingError,List[WordWithPosition]] = {
//    val sentenceAsListOfStrings: Array[String] = input.split(" ")
//    sentenceAsListOfStrings.indices.map(i => WordWithPosition(sentenceAsListOfStrings(i), i, i + 1)).toList
//  }

//  def convertToWords(input: String): Either[ParsingError,List[Word]] = {
//    val sentenceAsListOfStrings: Array[String] = input.split(" ")
//    sentenceAsListOfStrings.indices.map(i => Word(sentenceAsListOfStrings(i), i, i + 1)).toList
//  }

  def wordOverlap(fwords: Set[Word], awords: Set[Word]): Boolean = {
    for (fword <- fwords; aword <- awords if fword.start == aword.start)
      true
    false
  }

  def unify(df:Set[Feature], nf: Set[Feature]): Option[Set[Feature]] = {
    val wrongCombos: Set[(Feature,Feature)]=for {
      d <- df
      n <- nf
      if d.feature==n.feature
      if d.value!=n.value
    } yield {
      (d,n)
    }
    if (wrongCombos.nonEmpty) {
      println("Unification not possible because of error: "+wrongCombos)
      None
    } else {
      Some(df.union(nf))
    }
  }


}
