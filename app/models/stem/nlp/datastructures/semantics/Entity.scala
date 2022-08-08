package models.stem.nlp.datastructures.semantics

object Entity {

  def apply(name: String,
            single: String) =
  new Entity(name,Set(Set(single)))

  def apply(disjuncts: Set[Set[String]]) =
    new Entity("anonym",disjuncts, None)

  def apply(single: String) =
    new Entity("anonym",Set(Set(single)), None)
}

case class Entity(name: String,
                  disjuncts:Set[Set[String]],
                  filter: Option[String => Set[Set[Map[String, Set[String]]]] => Boolean] = None) extends Sem {

}
