package models.stem.nlp.datastructures.semantics

case class Quant(name: String, function: Pred => Entity) extends (Pred => Entity) with Sem {

  def apply(p: Pred): Entity = {
    p.filterFunction match {
      case None => function(p)
      case Some(filter) => {
        val e: Entity = function(p)
        Entity(e.name, e.disjuncts, Some(filter))
      }
    }
  }
}

object Quant {
  val alle: Pred => Entity = p => {
    val conjunction: Set[Set[String]] = Set(p.entities)
    val e: Entity = Entity(conjunction)
    e
  }

  val drie: Pred => Entity = p => {
    val powerset: Set[Set[String]] = p.entities.subsets().map(_.toSet).toSet
    val disjunction: Set[Set[String]] = powerset.filter(_.size == 3)
    val e: Entity = Entity(disjunction)
    e
  }

  val twee: Pred => Entity = p => {
    val powerset: Set[Set[String]] = p.entities.subsets().map(_.toSet).toSet
    val disjunction: Set[Set[String]] = powerset.filter(_.size == 2)
    val e: Entity = Entity(disjunction)
    e
  }

  val een: Pred => Entity = p => {
    val powerset: Set[Set[String]] = p.entities.subsets().map(_.toSet).toSet
    val disjunction: Set[Set[String]] = powerset.filter(_.size == 1)
    val e: Entity = Entity(disjunction)
    e
  }
}
