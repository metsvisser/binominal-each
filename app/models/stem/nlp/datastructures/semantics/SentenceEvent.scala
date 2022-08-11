package models.stem.nlp.datastructures.semantics

import models.stem.nlp.Conj
import models.stem.nlp.datastructures.syntax.Argument

case class SentenceEvent(name: String,
                         eventDisjunction: Set[Conj],
                         filter: Option[Set[Conj] => Boolean] = None
                        ) extends ((Entity, Argument) => SentenceEvent) with Sem {

  def crossCollConjunctionWithEventDisjunction(conjunction: Set[String],
                                               eventDisjunction: Set[Conj],
                                               role: Argument): Set[Conj] = {
    for {
      eventConjunction <- eventDisjunction
    } yield {
      crossCollConjunctionWithEventConjunction(conjunction, eventConjunction, role)
    }
  }

  def crossCollConjunctionWithEventConjunction(conjunction: Set[String],
                                               eventConjunction: Conj,
                                               role: Argument): Conj = {
    for {
      eventConjunct <- eventConjunction
    } yield {
      eventConjunct + (role.roleName -> conjunction)
    }
  }

  def crossCollDisjunctionWithEventDisjunction(disjuncts: Set[Set[String]],
                                               eventDisjunction: Set[Conj],
                                               role: Argument): SentenceEvent = {
    val tooDeep = for {
      conjunction <- disjuncts
    } yield {
      crossCollConjunctionWithEventDisjunction(conjunction, eventDisjunction, role)
    }
    SentenceEvent(name, tooDeep.flatten)
  }


  def incorporateConjunctIntoEventConjunction(conjunct: String,
                                              eventConjunction: Conj,
                                              role: Argument): Conj = {
    val newEventConjunction: Conj = for {
      eventConjunct <- eventConjunction
    } yield {
      eventConjunct + (role.roleName -> Set(conjunct))
    }
    newEventConjunction
  }

  def incorporateConjunctIntoEventDisjunction(conjunct: String,
                                              eventDisjunction: Set[Conj],
                                              role: Argument): Set[Conj] = {
    val eventDisjunctionsPerConjunct: Set[Conj] = for {
      eventConjunction <- eventDisjunction
    } yield {
      incorporateConjunctIntoEventConjunction(conjunct, eventConjunction, role)
    }
    eventDisjunctionsPerConjunct
  }

  def generator(x: List[List[Conj]]): List[List[Conj]] = x match {
    case Nil => List(Nil)
    case h :: _ => h.flatMap(i => generator(x.tail).map(i :: _))
  }

  def removeRangeRole(eventConjunction: Conj, role: Argument): Conj = {
    eventConjunction.map { event => event - role.roleName }
  }


  def crossIndConjunctionWithEventDisjunction(conjunction: Set[String],
                                              eventDisjunction: Set[Conj],
                                              role: Argument): Set[Conj] = {

    val setofEventDisjunctsForOneConjunct: Set[Set[Conj]] = for {
      conjunct <- conjunction
    } yield {
      incorporateConjunctIntoEventDisjunction(conjunct, eventDisjunction, role)
    }

    val listofEventDisjunctsForOneConjunct: List[List[Conj]] =
      setofEventDisjunctsForOneConjunct.toList.map(p => p.toList)

    val combineAllPossibleEventDisjuncts: Set[Set[Conj]] =
      generator(listofEventDisjunctsForOneConjunct).toSet.map((p: List[Conj]) => p.toSet)

    val combineAllPossibleEventDisjunctsFlattened: Set[Conj] =
      combineAllPossibleEventDisjuncts.map(_.flatten)

    filter match {
      case None =>
        combineAllPossibleEventDisjunctsFlattened
      case Some(filterFunction) =>
        val combineAllPossibleEventDisjunctsFiltered =
          combineAllPossibleEventDisjuncts.filter(ec => filterFunction(ec))
        val combineAllPossibleEventDisjunctsFilteredFlattened: Set[Conj] =
          combineAllPossibleEventDisjunctsFiltered.map(_.flatten)
        combineAllPossibleEventDisjunctsFilteredFlattened
    }
  }


  def crossIndDisjunctionWithEventDisjunction(disjuncts: Set[Set[String]],
                                              eventDisjunction: Set[Conj],
                                              role: Argument): SentenceEvent = {
    val newDisjuncts = for {
      conjunction <- disjuncts
    } yield {
      crossIndConjunctionWithEventDisjunction(conjunction, eventDisjunction, role)
    }
    SentenceEvent(name, newDisjuncts.flatten)
  }

  def apply(e: Entity, role: Argument): SentenceEvent = {
    val event =
      role.distribution match {
        case s if s == "coll" => {
          crossCollDisjunctionWithEventDisjunction(e.disjuncts, eventDisjunction, role)
        }
        case s if s == "ind" => {
          crossIndDisjunctionWithEventDisjunction(e.disjuncts, eventDisjunction, role)
        }
      }
    e.filter match {
      case None => event
      case Some(filterWithoutRole) => {
        val newFilter: Set[Conj] => Boolean = filterWithoutRole(role.roleName)
        SentenceEvent(event.name, event.eventDisjunction, Some(newFilter))
      }
    }
  }

  override def toString(): String = {
    eventDisjunction.mkString("\n")
  }
}

