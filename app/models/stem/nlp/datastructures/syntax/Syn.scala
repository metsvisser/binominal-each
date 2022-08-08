package models.stem.nlp.datastructures.syntax

object Syn {
  def apply(word: Word,
            pos: String,
            features:
            Set[Feature],
            args: Set[Argument]): Syn = {
    Syn(Set(word), pos, features, args)
  }
}

case class Syn(words: Set[Word],
                pos: String,
                features: Set[Feature] = Set.empty[Feature],
               args: Set[Argument] = Set.empty[Argument]) {

  def getNrOfWords(): Int = words.size

  def start(): Int = words.map(_.start).min

  def end(): Int = words.map(_.end).max


  override def toString: String = {

    var fea: String = ""
    if (noFea(features)) {
      fea = "no features"
    } else {
      for (f <- features) {
        fea = fea + f + " "
      }
    }
    "Sign: " + words + " POS:  " +
      pos +
      " FEATURES: " +
      fea +
      " ARGS: " +
      args
  }

  def noFea(v: Set[Feature]): Boolean = {
    if (v == null) true
    v.isEmpty
  }
}
