package meetup
package json

import org.scalatest._

class JsonOpsSpec extends FlatSpec with Matchers {
  import ast._
  import syntax._

  behavior of "json"

  it should "serialize" in {

    JsonObject(Map(
      "foo" -> JsonArray(List(
        JsonFalse,
        JsonString("1337")
      )),
      "bar" -> JsonNumber(BigDecimal(42)),
      "baz" -> JsonTrue,
      "qux" -> JsonNull
    )).toString should be ("""{"foo": [false, "1337"], "bar": 42, "baz": true, "qux": null}""")
  }

  it should "parse boilerplate free" in {
    import validation.Result.Valid

    case class Pet(name: String, legs: Int, awesome: Boolean)
    case class Person(name: String, age: Option[Int], pets: List[Pet])

    val expexted = Person("Bernd", Some(42),
      List(Pet("Ronny", 0, false), Pet("Rolf", 4, true)))

    parse.as[Person]("""
    {
      "name": "Bernd",
      "age": 42,
      "pets": [{
        "name": "Ronny",
        "legs": 0,
        "awesome": false
      }, {
        "name": "Rolf",
        "legs": 4,
        "awesome": true
      }]
    }
    """) should be(Valid(expexted))

    expexted.write.read[Person] should be(Valid(expexted))
  }
}
