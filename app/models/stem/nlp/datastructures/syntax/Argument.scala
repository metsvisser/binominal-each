package models.stem.nlp.datastructures.syntax

case class Argument(functorPos: String,
                    argPos: String,
                    resultPos: String,
                    unify: String,
                    position: String,
                    distribution: String = "ind",
                    roleName: String = ""
                   )
