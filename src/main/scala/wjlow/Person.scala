package wjlow

import scalaz.Scalaz._
import scalaz._

case class Person(name: String, age: Int) {

  def validate: Validation[NonEmptyList[String], Person] = {
    validateName +++ validateAge
  }

  private def validateName: Validation[NonEmptyList[String], Person] = {
    if (name === "Jack") this.successNel
    else "You are not Jack!".failureNel
  }

  private def validateAge: Validation[NonEmptyList[String], Person] = {
    if (age >= 18) this.successNel
    else "You are underaged!".failureNel
  }

  implicit val personSemigroup: Semigroup[Person] = Semigroup.instance((p1, p2) => p1)

}
