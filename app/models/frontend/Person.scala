package models.frontend

case class Person(name: String,
                  gender: String,
                  student: Boolean,
                  pets: Set[String]) {

}
