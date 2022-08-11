package models.stem.nlp.main

import models.stem.nlp.ParsingError
import models.stem.nlp.datastructures.semantics.{Entity, Filter, Pred, Quant, Sem, SentenceEvent}
import models.stem.nlp.datastructures.syntax.{Argument, Feature, Sign, Syn}
import models.stem.nlp.stem.nlp.utils.Utilities

class ShiftReduceParser extends ParsingAlgorithm {

  override def parseSigns(signs: Set[Sign]): Either[ParsingError, Set[Sign]] = {
    val stack = scala.collection.mutable.LinkedHashSet[Sign]()
    for (lex1 <- signs) {
      val added = scala.collection.mutable.LinkedHashSet[Sign]()
      added.add(lex1)
      var currentEqualsAfter: Boolean = false
      while (stack.nonEmpty && (!currentEqualsAfter)) {
        val distinctAddedBefore = added.groupBy(p => p.syn.words).view.mapValues(_.head)
        val currentAdded: Int = distinctAddedBefore.size
        val newlyCombined: Set[Sign] = reduce(stack.toSet, added.toSet)
        added.++=(newlyCombined)
        val distinctAddedAfter = added.groupBy(_.syn.words).view.mapValues(_.head)
        val afterAdded: Int = distinctAddedAfter.size
        if (currentAdded == afterAdded) {
          currentEqualsAfter = true
        }
      }
      stack.++=(added)
    }
    Right(stack.toSet)
  }


  // find all possible combinations in two sets of Signs
  def reduce(stack: Set[Sign], added: Set[Sign]): Set[Sign] = {
    val reduced = for {
      lex1 <- stack
      lex2 <- added
    } yield {
//      println("FUNC: " + lex1)
//      println("ARG: " + lex2)
      val combine1 = combineSigns(lex1, lex2)
      val combine2 = combineSigns(lex2, lex1)
      Set(combine1, combine2).flatten
    }
    reduced.flatten
  }

  def combineSyn(f: Syn, a: Syn, x: Argument): Option[Syn] = {
    if (a.pos == x.argPos &&
      checkPosition(f, a, x.position) &&
      checkFeatures(f, a, x.unify).nonEmpty
    ) Some(Syn(f.words ++ a.words,
      x.resultPos,
      checkFeatures(f, a, x.unify).getOrElse(Set.empty),
      f.args - x))
    else None
  }

  def combineSigns(f: Sign, a: Sign): Option[Sign] = {
    val set = for {
      arg <- f.syn.args
      syn <- combineSyn(f.syn, a.syn, arg)
      sem <- combineSem(f.sem, a.sem, arg)
    } yield {
      Sign(syn, sem)
    }
    set.headOption
  }

  def checkPosition(f: Syn, a: Syn, position: String): Boolean = {
    if (position.==("ir") && f.end() != a.start()) return false
    if (position.==("r") && f.end() > a.start()) return false
    if (position.==("l") && f.start() < a.end()) return false
    if (position.==("il") && f.start() != a.end()) return false
    true
  }

  private def checkFeatures(f: Syn, a: Syn, unify: String): Option[Set[Feature]] = {
    if (unify == "unify") Utilities.unify(f.features, a.features)
    else if (unify == "functor") Some(f.features)
    else None
  }

  def combineSem(fun: Sem, arg: Sem, r: Argument): Option[Sem] = {
    (fun, arg) match {
      case (q: Quant, p: Pred) => Some(q(p))
      case (t: Filter, p: Pred) if p.filterFunction.isEmpty => Some(t(p))
      case (v: SentenceEvent, e: Entity) => Some(v(e, r))
      case (_, _) => None
    }
  }
}
