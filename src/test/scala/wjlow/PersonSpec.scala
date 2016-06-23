package wjlow

import org.scalacheck.Gen
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSpec, Matchers}

import scalaz._

class PersonSpec extends FunSpec with TypeCheckedTripleEquals with Matchers with PropertyChecks {

  val validAgeGen = Gen.choose(18, 100)
  val invalidAgeGen = Gen.choose(0, 17)
  val invalidNameGen = Gen.numStr

  describe("Person.validate") {

    it("should return Success for all valid Persons") {
      forAll(validAgeGen) { (age: Int) =>
        val person = Person("Jack", age)
        person.validate should ===(Success(person))
      }
    }

    it("should return Failure for valid Name but invalid Age") {
      forAll(invalidAgeGen) { (age: Int) =>
        Person("Jack", age).validate should ===(Failure(NonEmptyList("You are underaged!")))
      }
    }

    it("should return Failure for invalid Name but valid Age") {
      forAll(invalidNameGen, validAgeGen) { (name: String, age: Int) =>
        Person(name, age).validate should ===(Failure(NonEmptyList("You are not Jack!")))
      }
    }


    it("should return Failure for invalid Name and invalid Age") {
      forAll(invalidNameGen, invalidAgeGen) { (name: String, age: Int) =>
        Person(name, age).validate should ===(Failure(NonEmptyList("You are not Jack!", "You are underaged!")))
      }
    }

  }

}
