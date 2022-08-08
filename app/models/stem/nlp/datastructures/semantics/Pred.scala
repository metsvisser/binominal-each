package models.stem.nlp.datastructures.semantics

case class Pred(name: String,
                entities: Set[String],
                filterFunction: Option[String => Set[Set[Map[String, Set[String]]]] => Boolean]) extends Sem {


  override def toString(): String = "[" + name + ": " + entities + "] filter: " + filterFunction

}

object Pred {

  def apply(name: String,
            entities: Set[String]): Pred = Pred(name, entities, None)
}
