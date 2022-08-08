package models.stem.nlp.datastructures.semantics

  //
  //  def theSame(ecc: Set[Set[Map[String, Set[String]]]], dist: Set[String]): Boolean = {
  //    println("INSIDE SAME")
  //    println("ECC: "+ecc)
  //    val firstMapping: Map[String, Set[String]]=ecc.toList.head.toList.head
  //    println("FirstEC: "+firstMapping)
  //    val distRoleOpt=firstMapping.find(value => value._2 subsetOf dist)
  //    val role: String=distRoleOpt match {
  //      case Some(x)=> x._1
  //      case None => "roleNotFound"
  //    }
  //    println("Roleee: "+role)
  //    val firstEc: Set[Map[String,Set[String]]]=ecc.toList.head
  //    val setOfDistsFirstConjunct: Set[Set[String]]=firstEc.map(mapping => mapping(role))
  //
  //    ecc.forall {ec: Set[Map[String,Set[String]]] =>
  //      val SetOfDists: Set[Set[String]]=ec.map(mapping => mapping(role))
  //      SetOfDists==setOfDistsFirstConjunct
  //    }
  //  }

  //  def theSame(ecc: Set[Set[Map[String, Set[String]]]], role: Role): Boolean = {
  //    println("INSIDE SAME")
  //    println("ECC: "+ecc)
  //    val setOfMaps: Set[Map[String,Set[String]]]=ecc.toList.head.map(event => event-role.name)
  //    println("setOfMaps: "+setOfMaps)
  //
  //    ecc.forall {ec: Set[Map[String,Set[String]]] =>
  //      val removedRangeConjunct: Set[Map[String,Set[String]]]=ec.map(event => event-role.name)
  //      removedRangeConjunct==setOfMaps
  //    }
  //  }


