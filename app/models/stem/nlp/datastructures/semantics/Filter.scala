package models.stem.nlp.datastructures.semantics

case class Filter(name: String, filterFunction: String => Set[Set[Map[String, Set[String]]]] => Boolean) extends (Pred => Pred) with Sem {

  def apply(p: Pred): Pred = {
    Pred(p.name, p.entities, Some(filterFunction))
  }

}

object Filter {

  val dezelfde: String => Set[Set[Map[String, Set[String]]]] => Boolean = role => { ecc =>
    ecc.groupBy(ec => ec.map(mapping => mapping(role))).size==1
  }

  val verschillende: String => Set[Set[Map[String, Set[String]]]] => Boolean = role => { ecc =>
    ecc.groupBy(ec => ec.map(mapping => mapping(role))).size==ecc.size
  }

}
