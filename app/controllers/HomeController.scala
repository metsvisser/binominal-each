package controllers

import javax.inject._
import play.api.mvc._
import models.stem.nlp.main.{DutchLanguage, Parser}
import models.frontend.Person
import models.stem.nlp.{Conj, ParsingError}
import models.stem.nlp.datastructures.models.ModelParser
import models.stem.nlp.datastructures.semantics.Sem

@Singleton
class HomeController @Inject()(cc: ControllerComponents,
                              ) extends AbstractController(cc) {

  val modelParser = new ModelParser
  val dutchLanguage = new DutchLanguage
  val parser = new Parser(dutchLanguage)

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index("Boniminal each",
      dutchLanguage.vocabulary.items.map(l => (l.word, l.pos))))
  }

  def parseSentence: Action[AnyContent] = Action { request =>
    val form: Option[Map[String, Seq[String]]]=request.body.asFormUrlEncoded
    form match {
      case Some(queryString) =>
        val sentence=queryString("sentence").head.toLowerCase
        val (people, pets): (Set[Person],Set[String])=modelParser.convertQueryString(queryString)
        val model: Set[Sem]=modelParser.convertToModel(people, pets)
        val zinsem: Either[ParsingError,Option[Conj]] = parser.parseSentence(sentence,model)
        zinsem match {
          case Left(x) => Ok(s"Parsing error: ${x.msg}")
          case Right(y) => y match {
            case Some(tt) => Ok(s"True: $tt")
            case None => Ok("False")
          }
        }
      case None => Ok("Form not found")
    }
  }

}
