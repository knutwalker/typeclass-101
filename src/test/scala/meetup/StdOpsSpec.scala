package meetup
package std

import org.scalatest._

class StdOpsSpec extends FlatSpec with Matchers with NonImplicitAssertions {

  behavior of "Equality"

  it should "provide type-safe equality" in {

    ("foo" ≡ "foo") should be(true)
    ("foo" ≡ "bar") should be(false)
    """("foo" ≡ 42)""" shouldNot typeCheck
  }

  behavior of "Comparable"

  it should "provide type-safe comparing" in {

    ("foo" ≤ "foo") should be(true)
    ("foo" ≤ "bar") should be(false)
    """("foo" ≤ 42)""" shouldNot typeCheck
  }

  it should "provide all other comparing methods" in {

    (Foo(42) ≤ Foo(42)) should be(true)
    (Foo(42) < Foo(42)) should be(false)
    (Foo(42) ≥ Foo(42)) should be(true)
    (Foo(42) > Foo(42)) should be(false)
    (Foo(42) ≡ Foo(42)) should be(true)
    (Foo(42) ≠ Foo(42)) should be(false)

    (Foo(42) ≤ Foo(1337)) should be(true)
    (Foo(42) < Foo(1337)) should be(true)
    (Foo(42) ≥ Foo(1337)) should be(false)
    (Foo(42) > Foo(1337)) should be(false)
    (Foo(42) ≡ Foo(1337)) should be(false)
    (Foo(42) ≠ Foo(1337)) should be(true)
  }
}

case class Foo(x: Int)
object Foo {
  implicit val comparableFoo: Comparable[Foo] = new Comparable[Foo] {
    def lte(x: Foo, y: Foo): Boolean =
      x.x ≤ y.x
  }
}
