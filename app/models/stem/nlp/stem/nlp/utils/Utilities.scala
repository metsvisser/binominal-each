package models.stem.nlp.stem.nlp.utils

import models.stem.nlp.datastructures.syntax._
import models.stem.nlp.{Conj, ParsingError}


import scala.collection.mutable.LinkedHashSet

object Utilities {

  def processFeatures(featureString: String): Set[Feature] = {
    if (featureString.trim == "x") Set.empty[Feature]
    else {
      val featureStrings: Array[String] = featureString.split(";")
      val features = (for {
        featureString <- featureStrings
      } yield {
        val splitFeature: Array[String] = featureString.split("=")
        Feature(splitFeature(0), splitFeature(1))
      }).toSet
      features
    }
  }

  def unify(df: Set[Feature], nf: Set[Feature]): Option[Set[Feature]] = {
    val wrongCombos: Set[(Feature, Feature)] = for {
      d <- df
      n <- nf
      if d.feature == n.feature
      if d.value != n.value
    } yield {
      (d, n)
    }
    if (wrongCombos.nonEmpty) {
      println("Unification not possible because of error: " + wrongCombos)
      None
    } else {
      Some(df.union(nf))
    }
  }
}
