package meetup

import validation.Result
import validation.Result._

import shapeless._
import shapeless.labelled._

import language.dynamics

package object json {
  object ast {
    sealed abstract class Json extends Product with Serializable with Dynamic {
      def selectDynamic(field: String): Json = this match {
        case JsonObject(xs) => xs get field getOrElse JsonNull
        case _              => JsonNull
      }

      def applyDynamic(field: String)(idx: Int) = selectDynamic(field) match {
        case JsonArray(xs) => xs.drop(idx).headOption getOrElse JsonNull
        case _             => JsonNull
      }
    }

    case object JsonNull extends Json {
      override def toString = "null"
    }

    case class JsonString(value: String) extends Json {
      override def toString = '"' + value + '"'
    }

    case class JsonNumber(value: BigDecimal) extends Json {
     override def toString = value.toString
    }

    sealed abstract class JsonBool extends Json
    object JsonBool {
      def apply(x: Boolean) = if (x) JsonTrue else JsonFalse
      def unapply(j: JsonBool): Option[Boolean] = Some(j == JsonTrue)
    }
    case object JsonTrue extends JsonBool {
      override def toString = "true"
    }
    case object JsonFalse extends JsonBool {
      override def toString = "false"
    }

    case class JsonObject(fields: Map[String, Json] = Map.empty) extends Json {
      private final val quote = "\""
      override def toString = fields.map(pair => s"${quote}${pair._1}${quote}: ${pair._2}").mkString("{", ", ", "}")
    }

    case class JsonArray(elements: List[Json] = List.empty) extends Json {
      override def toString = elements.mkString("[", ", ", "]")
    }
  }

  object ops {
    import ast._

    trait JsonRead[A] {
      def read(j: Json): Result[String, A]
      def missing: Option[A] = None
    }

    trait JsonWrite[A] {
      def write(a: A): Json
    }

    object JsonRead extends auto.JsonReadDerived {

      implicit object jsonReadInt extends JsonRead[Int] {
        def read(j: Json) = j match {
          case JsonNumber(n) if n.isValidInt => valid(n.toInt)
          case JsonNumber(n)                 => invalid(s"$n is a number but not a valid int.")
          case x                             => invalid(s"$x is not a number.")
        }
      }

      implicit object jsonReadString extends JsonRead[String] {
        def read(j: Json) = j match {
          case JsonString(s) => valid(s)
          case x             => invalid(s"$x is not a string.")
        }
      }

      implicit object jsonReadBool extends JsonRead[Boolean] {
        def read(j: Json) = j match {
          case JsonBool(b) => valid(b)
          case x           => invalid(s"$x is not a boolean.")
        }
      }
    }

    object JsonWrite extends auto.JsonWriteDerived {

      implicit object jsonWriteInt extends JsonWrite[Int] {
        def write(a: Int) = JsonNumber(BigDecimal(a))
      }

      implicit object jsonWriteString extends JsonWrite[String] {
        def write(a: String) = JsonString(a)
      }

      implicit object jsonWriteBool extends JsonWrite[Boolean] {
        def write(a: Boolean) = JsonBool(a)
      }
    }
  }

  object syntax {
    import ops._
    import ast._

    implicit final class JsonReadOps(val j: Json) extends AnyVal {
      def read[A](implicit A: JsonRead[A]): Result[String, A] =
        A.read(j)
    }

    implicit final class JsonWriteOps[A](val x: A) extends AnyVal {
      def write(implicit A: JsonWrite[A]): Json =
        A.write(x)
    }
  }

  object auto {
    import ast._
    import ops._
    import syntax._

    trait JsonReadDerived {
      def apply[A](implicit A: JsonRead[A]): JsonRead[A] = A

      implicit def derive[A, Repr](implicit
        gen: LabelledGeneric.Aux[A, Repr],
        repr: Lazy[JsonRead[Repr]])
      : JsonRead[A] = new JsonRead[A] {
        def read(j: Json) = repr.value.read(j).map(gen.from)
      }

      implicit val proveHNil: JsonRead[HNil] = new JsonRead[HNil] {
        def read(j: Json) = valid(HNil)
      }

      implicit def proveHCons[K <: Symbol, H, T <: HList](implicit
        K: Witness.Aux[K],
        H: Lazy[JsonRead[H]],
        T: Lazy[JsonRead[T]])
      : JsonRead[FieldType[K, H] :: T] = new JsonRead[FieldType[K, H] :: T] {
        def read(j: Json) = j match {
          case JsonObject(xs) =>
            xs.get(K.value.name) match {
              case Some(fj) => H.value.read(fj).and(T.value.read(j)) apply (field[K](_) :: _)
              case None     => H.value.missing match {
                case Some(v) => T.value.read(j).map(field[K](v) :: _)
                case None    => invalid(s"the field ${K.value.name} is missing")
              }
            }
          case x => invalid(s"$j is not a record type (object)")
        }
      }

      implicit def jsonReadOption[A: JsonRead]: JsonRead[Option[A]] = new JsonRead[Option[A]] {
        def read(j: Json) = j match {
          case JsonNull => valid(None)
          case x        => JsonRead[A].read(x).map(Some(_))
        }
        override def missing = Some(None)
      }

      implicit def jsonReadList[A: JsonRead]: JsonRead[List[A]] = new JsonRead[List[A]] {
        def read(j: Json) = j match {
          case JsonArray(xs) => traverse(xs)(JsonRead[A].read)
          case x             => invalid(s"$x is not a collection")
        }
        override def missing = Some(Nil)
      }
    }

    trait JsonWriteDerived {
      def apply[A](implicit A: JsonWrite[A]): JsonWrite[A] = A

      implicit def derive[A, Repr](implicit
        gen: LabelledGeneric.Aux[A, Repr],
        repr: Lazy[JsonWrite[Repr]])
      : JsonWrite[A] = new JsonWrite[A] {
        def write(a: A) = repr.value.write(gen.to(a))
      }

      implicit val proveHNil: JsonWrite[HNil] = new JsonWrite[HNil] {
        def write(a: HNil) = JsonNull
      }

      implicit def proveHCons[K <: Symbol, H, T <: HList](implicit
        K: Witness.Aux[K],
        H: Lazy[JsonWrite[H]],
        T: Lazy[JsonWrite[T]])
      : JsonWrite[FieldType[K, H] :: T] = new JsonWrite[FieldType[K, H] :: T] {
        def write(a: FieldType[K, H] :: T) = {
          val head = H.value.write(a.head)
          T.value.write(a.tail) match {
            case JsonObject(xs) => JsonObject(xs.updated(K.value.name, head))
            case _              => JsonObject(Map(K.value.name -> head))
          }
        }
      }

      implicit def jsonWriteOption[A: JsonWrite]: JsonWrite[Option[A]] = new JsonWrite[Option[A]] {
        def write(a: Option[A]) = a match {
          case Some(v)  => v.write
          case _        => JsonNull
        }
      }

      implicit def jsonWriteList[A: JsonWrite]: JsonWrite[List[A]] = new JsonWrite[List[A]] {
        def write(a: List[A]) = JsonArray(a.map(_.write))
      }
    }
  }

  object parse {
    import jawn._
    import ast._
    import ops._
    import syntax._

    def apply(x: String): Json =
      Parser.parseUnsafe(x)(JsonFacade)

    def as[A: JsonRead](x: String): Result[String, A] =
      apply(x).read[A]

    object JsonFacade extends SimpleFacade[Json] {
      def jnull() = JsonNull
      def jfalse() = JsonFalse
      def jtrue() = JsonTrue
      def jnum(s: String) = JsonNumber(BigDecimal(s))
      def jint(s: String) = JsonNumber(BigDecimal(s))
      def jstring(s: String) = JsonString(s)
      def jarray(vs: List[Json]) = JsonArray(vs)
      def jobject(vs: Map[String, Json]) = JsonObject(vs)
    }
  }
}
