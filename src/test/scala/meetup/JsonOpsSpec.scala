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
}
