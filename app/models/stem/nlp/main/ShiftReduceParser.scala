package models.stem.nlp.main

import models.stem.nlp.ParsingError
import models.stem.nlp.datastructures.semantics.{Entity, Filter, Pred, Quant, Sem, SentenceEvent}
import models.stem.nlp.datastructures.syntax.{Argument, Feature, Sign, Syn}
import models.stem.nlp.stem.nlp.utils.Utilities

class ShiftReduceParser extends ParsingAlgorithm {

  override def parseSigns(signs: Set[Sign]): Either[ParsingError, Set[Sign]] = {
    //        println("---Start parsing")
    val stack = scala.collection.mutable.LinkedHashSet[Sign]()
    for (lex1 <- signs) {
      val added = scala.collection.mutable.LinkedHashSet[Sign]()
      added.add(lex1)
      //            println("------Shift: " + lex1)
      var currentEqualsAfter: Boolean = false
      while (stack.nonEmpty && (!currentEqualsAfter)) {
        //        println("currentAdded: "+currentAdded)
        val distinctAddedBefore = added.groupBy(p => p.syn.words).view.mapValues(_.head)
        val currentAdded: Int = distinctAddedBefore.size


        val newlyCombined: Set[Sign] = reduce(stack.toSet, added.toSet)
        added.++=(newlyCombined)
        val distinctAddedAfter = added.groupBy(_.syn.words).view.mapValues(_.head)

        val afterAdded: Int = distinctAddedAfter.size
        //        println("afterAdded: "+afterAdded)

        if (currentAdded == afterAdded) {
          currentEqualsAfter = true
        }
      }
      stack.++=(added)
      //            println("STACKSIZE: " + stack.size)
      stack.foreach(println)
    }
    Right(stack.toSet)
  }


  // find all possible combinations in two sets of Signs
  def reduce(stack: Set[Sign], added: Set[Sign]): Set[Sign] = {
    val reduced: Set[Set[Option[Sign]]] = for {
      lex1 <- stack
      lex2 <- added
    } yield {
      println("FUNC: " + lex1)
      println("ARG: " + lex2)
      val combine1 = combineSigns(lex1, lex2)
      //      println("combine1: "+combine1)
      val combine2 = combineSigns(lex2, lex1)
      //      println("combine2: "+combine2)
      val setCOmbines = Set(combine1, combine2)
      setCOmbines.foreach(p => println("RES: " + p))
      //      println("setCOmbines: "+setCOmbines)
      setCOmbines
      //      Set(combine(lex1, lex2), combine(lex2, lex1))
    }
    val flattened = reduced.flatten.flatten
    //    stack.foreach(p => println("stack: "+p))
    //    added.foreach(p => println("added: "+p))
    //    println("reduced: "+reduced)
    //    println("flattened: "+flattened)
    //    println("flattenedSize: "+flattened.size)
    flattened
  }

  def combineSyn(f: Syn, a: Syn, x: Argument): Option[Syn] = {
    x match {
      case arg: Argument
        if a.pos == x.argPos &&
          checkPosition(f, a, x.position) &&
          checkFeatures(f, a, x.unify).nonEmpty => {
        Some(Syn(f.words ++ a.words,
          arg.resultPos,
          checkFeatures(f, a, x.unify).getOrElse(Set.empty),
          f.args - arg))
      }
      case _ => None
    }
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
    unify match {
      case u if u == "unify" =>
        Utilities.unify(f.features, a.features)
      case u if u == "functor" =>
        Some(f.features)
      case _ => None
    }
  }

  def combineSem(fun: Sem, arg: Sem, r: Argument): Option[Sem] = {
    (fun, arg) match {
      case (q: Quant, p: Pred) => Some(q(p))
      case (t: Filter, p: Pred) =>
        p.filterFunction match {
          case Some(_) => None
          case None => Some(t(p))
        }
      case (v: SentenceEvent, e: Entity) =>
        println("herree")
        Some(v(e, r))
      case (_, _) => None
    }
  }
}
