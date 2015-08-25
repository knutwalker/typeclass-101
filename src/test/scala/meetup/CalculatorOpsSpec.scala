package meetup
package calcops

import org.scalatest._

class CalculatorOpsSpec extends FunSpec with Matchers {

  describe("oop style operands") {
    import oop._

    it("should add to ints") {
      (Int32(42) add Int32(1337)) should equal (Int32(42 + 1337))
    }

    it("should not allow to add ints and reals") {
      "Int32(42) add Real32(1337.0f)" shouldNot typeCheck
    }

    it("should multiply to reals") {
      (Real32(42.0f) mult Real32(1337.0f)) should equal (Real32(42.0f * 1337.0f))
    }

    it("should not allow to multiply reals and ints") {
      "Real32(42.0f) mult Int32(1337)" shouldNot typeCheck
    }
  }

  describe("pattern style operands") {
    import pattern._

    it("should add to ints") {
      add(Int32(42))(Int32(1337)) should equal (Int32(42 + 1337))
    }

    it("should not allow to add ints and reals") {
      "add(Int32(42))(Real32(1337.0f))" shouldNot typeCheck
    }

    it("should multiply to reals") {
      mult(Real32(42.0f))(Real32(1337.0f)) should equal (Real32(42.0f * 1337.0f))
    }

    it("should not allow to multiply reals and ints") {
      "mult(Real32(42.0f))(Int32(1337))" shouldNot typeCheck
    }
  }

  describe("typeclass style operands") {
    import typeclass._

    it("should add to ints") {
      (Int32(42) add Int32(1337)) should equal (Int32(42 + 1337))
    }

    it("should not allow to add ints and reals") {
      "(Int32(42) add Real32(1337.0f))" shouldNot typeCheck
    }

    it("should multiply to reals") {
      mult(Real32(42.0f), Real32(1337.0f)) should equal (Real32(42.0f * 1337.0f))
    }

    it("should not allow to multiply reals and ints") {
      "mult(Real32(42.0f), Int32(1337))" shouldNot typeCheck
    }
  }
}
