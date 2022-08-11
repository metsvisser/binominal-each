package models.stem.nlp.datastructures.models

import models.frontend.Person
import models.stem.nlp.Conj
import models.stem.nlp.datastructures.semantics.{Entity, ModelEvent, Pred, Sem}

class ModelParser {

  def convertToModel(people: Set[Person],
                     pets: Set[String]): Set[Sem] = {

    // manually created denotations
    val student = Pred("student", people.filter(_.student).map(_.name))
    val woman = Pred("vrouw", people.filter(_.gender=="female").map(_.name))
    val man = Pred("man", people.filter(_.gender=="male").map(_.name))

    // Create entities for every person
    val personEntities: Set[Sem]= people.map(_.name).map(n => Entity(n, n))

    // Create entities and pets for every pet
    val petEntities: Set[Sem]= pets.map(_+"1").map(n => Entity(n, n))
    val petPreds: Set[Sem] = pets.map(n => Pred(n,Set(n)))
    val huisdier = Pred("huisdier", pets)

    // for Event denotations, extract the info from the input model
    val conjunction: Conj = for {
      person <- people
      pet <- person.pets
    } yield {
      Map("bezitter" -> Set(person.name), "bezat" -> Set(pet))
    }
    val hebben = ModelEvent("hebben", conjunction)

    personEntities.union(petEntities).union(petPreds).union(Set(huisdier, man, woman, student, hebben))

  }

  def convertQueryString(querystring: Map[String, Seq[String]]): (Set[Person],Set[String]) = {
    val pets: Set[String]=querystring.get("pets").fold(Set.empty[String])(p => p(0).split(",").toSet)
    val names: Set[String]=querystring.keySet.filter(_.contains("name")).map( s => s.substring(0, s.indexOf('-')))
    val people:Set[Person]=for {
      name <- names
    } yield {
      Person(name,
        querystring.get(name+"-gender").fold("undefined")((p: Seq[String]) => p(0)),
        querystring.get(name+"-student").fold(false)((p: Seq[String]) => p(0).toBoolean),
        querystring.keySet.filter(_.contains(name+"-owns-")).map( s => s.substring(s.lastIndexOf("-")+1)))
    }
    (people, pets)
  }

}
