package models.stem.nlp.datastructures.semantics

import models.stem.nlp.Conj
import models.stem.nlp.datastructures.syntax.Argument

case class SentenceEvent(name: String,
                         eventDisjunction: Set[Set[Map[String, Set[String]]]],
                         filter: Option[Set[Set[Map[String, Set[String]]]] => Boolean] = None
                        ) extends ((Entity, Argument) => SentenceEvent) with Sem {

  def crossCollConjunctionWithEventDisjunction(conjunction: Set[String],
                                               eventDisjunction: Set[Set[Map[String, Set[String]]]],
                                               role: Argument): Set[Set[Map[String, Set[String]]]] = {
    for {
      eventConjunction <- eventDisjunction
    } yield {
      crossCollConjunctionWithEventConjunction(conjunction, eventConjunction, role)
    }
  }

  def crossCollConjunctionWithEventConjunction(conjunction: Set[String],
                                               eventConjunction: Set[Map[String, Set[String]]],
                                               role: Argument): Set[Map[String, Set[String]]] = {
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
                                              role: Argument): Set[Map[String, Set[String]]] = {
    val newEventConjunction: Set[Map[String, Set[String]]] = for {
      eventConjunct <- eventConjunction
    } yield {
      eventConjunct + (role.roleName -> Set(conjunct))
    }
    newEventConjunction
  }

  def incorporateConjunctIntoEventDisjunction(conjunct: String,
                                              eventDisjunction: Set[Conj],
                                              role: Argument): Set[Set[Map[String, Set[String]]]] = {
    val eventDisjunctionsPerConjunct: Set[Set[Map[String, Set[String]]]] = for {
      eventConjunction <- eventDisjunction
    } yield {
      incorporateConjunctIntoEventConjunction(conjunct, eventConjunction, role)
    }
    eventDisjunctionsPerConjunct
  }

  def generator(x: List[List[Set[Map[String, Set[String]]]]]): List[List[Set[Map[String, Set[String]]]]] = x match {
    case Nil => List(Nil)
    case h :: _ => h.flatMap(i => generator(x.tail).map(i :: _))
  }

  def removeRangeRole(eventConjunction: Set[Map[String, Set[String]]], role: Argument): Set[Map[String, Set[String]]] = {
    eventConjunction.map { event => event - role.roleName }
  }


  def crossIndConjunctionWithEventDisjunction(conjunction: Set[String],
                                              eventDisjunction: Set[Set[Map[String, Set[String]]]],
                                              role: Argument): Set[Set[Map[String, Set[String]]]] = {

    val setofEventDisjunctsForOneConjunct: Set[Set[Set[Map[String, Set[String]]]]] = for {
      conjunct <- conjunction
    } yield {
      incorporateConjunctIntoEventDisjunction(conjunct, eventDisjunction, role)
    }

    //    val flattenedSet=setofEventDisjunctsForOneConjunct.flatten
    //    println("Size of flattened set: "+flattenedSet.size)
    //    flattenedSet.foreach(p => println("Flattened subset: "+p))


    val listofEventDisjunctsForOneConjunct: List[List[Set[Map[String, Set[String]]]]] = setofEventDisjunctsForOneConjunct.toList.map(p => p.toList)
    val combineAllPossibleEventDisjuncts: Set[Set[Set[Map[String, Set[String]]]]] = generator(listofEventDisjunctsForOneConjunct).toSet.map((p: List[Set[Map[String, Set[String]]]]) => p.toSet)
    //    println("begintHier: "+combineAllPossibleEventDisjuncts.size)
    //    combineAllPossibleEventDisjuncts.foreach(p => println("subset: "+p))
    val combineAllPossibleEventDisjunctsFlattened: Set[Set[Map[String, Set[String]]]] = combineAllPossibleEventDisjuncts.map(_.flatten)

    filter match {
      case None => {
        //        println("No filter function found")
        combineAllPossibleEventDisjunctsFlattened
      }
      case Some(filterFunction) => {
        val combineAllPossibleEventDisjunctsFiltered = combineAllPossibleEventDisjuncts.filter(ec => filterFunction(ec))
        val combineAllPossibleEventDisjunctsFilteredFlattened: Set[Set[Map[String, Set[String]]]] = combineAllPossibleEventDisjunctsFiltered.map(_.flatten)
        //        println("FilteredSame: "+combineAllPossibleEventDisjunctsFilteredFlattened.size)
        //        combineAllPossibleEventDisjunctsFilteredFlattened.foreach(p => println("Same: "+p))
        combineAllPossibleEventDisjunctsFilteredFlattened
      }
    }

    //    combineAllPossibleEventDisjunctsFlattened
  }


  def crossIndDisjunctionWithEventDisjunction(disjuncts: Set[Set[String]],
                                              eventDisjunction: Set[Set[Map[String, Set[String]]]],
                                              role: Argument): SentenceEvent = {
    val tooDeep = for {
      conjunction <- disjuncts
    } yield {
      crossIndConjunctionWithEventDisjunction(conjunction, eventDisjunction, role)
    }

    val newEvent = SentenceEvent(name, tooDeep.flatten)
    //    println("NewEvent: "+newEvent)
    newEvent
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
        val newFilter: Set[Set[Map[String, Set[String]]]] => Boolean = filterWithoutRole(role.roleName)
        SentenceEvent(event.name, event.eventDisjunction, Some(newFilter))
      }
    }
  }

  override def toString(): String = {
    //    println("Number of disjuncts: " + eventDisjunction.size)
    eventDisjunction.mkString("\n")
  }
}

