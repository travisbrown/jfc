/*
 * Copyright 2024 circe
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.circe

import cats.kernel.Eq
import cats.kernel.instances.all.*
import cats.syntax.eq.*
import io.circe.{ Codec, Decoder, Encoder, Json }
import io.circe.testing.CodecTests
import io.circe.tests.CirceMunitSuite
import org.scalacheck.{ Arbitrary, Gen }

object DerivesSuite {
  case class Box[A](a: A) derives Decoder, Encoder

  object Box {
    implicit def eqBox[A: Eq]: Eq[Box[A]] = Eq.by(_.a)
    implicit def arbitraryBox[A](implicit A: Arbitrary[A]): Arbitrary[Box[A]] = Arbitrary(A.arbitrary.map(Box(_)))
  }

  case class Qux[A](i: Int, a: A, j: Int) derives Codec

  object Qux {
    implicit def eqQux[A: Eq]: Eq[Qux[A]] = Eq.by(q => (q.i, q.a, q.j))

    implicit def arbitraryQux[A](implicit A: Arbitrary[A]): Arbitrary[Qux[A]] =
      Arbitrary(
        for {
          i <- Arbitrary.arbitrary[Int]
          a <- A.arbitrary
          j <- Arbitrary.arbitrary[Int]
        } yield Qux(i, a, j)
      )
  }

  case class Wub(x: Long) derives Codec.AsObject

  object Wub {
    implicit val eqWub: Eq[Wub] = Eq.by(_.x)
    implicit val arbitraryWub: Arbitrary[Wub] = Arbitrary(Arbitrary.arbitrary[Long].map(Wub(_)))
  }

  sealed trait Foo derives Codec.AsObject
  case class Bar(i: Int, s: String) extends Foo
  case class Baz(xs: List[String]) extends Foo
  case class Bam(w: Wub, d: Double) extends Foo derives Codec.AsObject

  object Bar {
    implicit val eqBar: Eq[Bar] = Eq.fromUniversalEquals
    implicit val arbitraryBar: Arbitrary[Bar] = Arbitrary(
      for {
        i <- Arbitrary.arbitrary[Int]
        s <- Arbitrary.arbitrary[String]
      } yield Bar(i, s)
    )

    implicit val decodeBar: Decoder[Bar] = Decoder.forProduct2("i", "s")(Bar.apply)
    implicit val encodeBar: Encoder[Bar] = Encoder.forProduct2("i", "s") {
      case Bar(i, s) => (i, s)
    }
  }

  object Baz {
    implicit val eqBaz: Eq[Baz] = Eq.fromUniversalEquals
    implicit val arbitraryBaz: Arbitrary[Baz] = Arbitrary(
      Arbitrary.arbitrary[List[String]].map(Baz.apply)
    )

    implicit val decodeBaz: Decoder[Baz] = Decoder[List[String]].map(Baz(_))
    implicit val encodeBaz: Encoder[Baz] = Encoder.instance {
      case Baz(xs) => Json.fromValues(xs.map(Json.fromString))
    }
  }

  object Bam {
    implicit val eqBam: Eq[Bam] = Eq.fromUniversalEquals
    implicit val arbitraryBam: Arbitrary[Bam] = Arbitrary(
      for {
        w <- Arbitrary.arbitrary[Wub]
        d <- Arbitrary.arbitrary[Double]
      } yield Bam(w, d)
    )
  }

  object Foo {
    implicit val eqFoo: Eq[Foo] = Eq.fromUniversalEquals

    implicit val arbitraryFoo: Arbitrary[Foo] = Arbitrary(
      Gen.oneOf(
        Arbitrary.arbitrary[Bar],
        Arbitrary.arbitrary[Baz],
        Arbitrary.arbitrary[Bam]
      )
    )
  }

  sealed trait RecursiveAdtExample derives Codec.AsObject
  case class BaseAdtExample(a: String) extends RecursiveAdtExample derives Codec.AsObject
  case class NestedAdtExample(r: RecursiveAdtExample) extends RecursiveAdtExample derives Codec.AsObject

  object RecursiveAdtExample {
    implicit val eqRecursiveAdtExample: Eq[RecursiveAdtExample] = Eq.fromUniversalEquals

    private def atDepth(depth: Int): Gen[RecursiveAdtExample] = if (depth < 3)
      Gen.oneOf(
        Arbitrary.arbitrary[String].map(BaseAdtExample(_)),
        atDepth(depth + 1).map(NestedAdtExample(_))
      )
    else Arbitrary.arbitrary[String].map(BaseAdtExample(_))

    implicit val arbitraryRecursiveAdtExample: Arbitrary[RecursiveAdtExample] =
      Arbitrary(atDepth(0))
  }

  case class RecursiveWithOptionExample(o: Option[RecursiveWithOptionExample]) derives Codec.AsObject

  object RecursiveWithOptionExample {
    implicit val eqRecursiveWithOptionExample: Eq[RecursiveWithOptionExample] =
      Eq.fromUniversalEquals

    private def atDepth(depth: Int): Gen[RecursiveWithOptionExample] = if (depth < 3)
      Gen.oneOf(
        Gen.const(RecursiveWithOptionExample(None)),
        atDepth(depth + 1)
      )
    else Gen.const(RecursiveWithOptionExample(None))

    implicit val arbitraryRecursiveWithOptionExample: Arbitrary[RecursiveWithOptionExample] =
      Arbitrary(atDepth(0))
  }

  enum Vegetable derives Codec:
    case Potato(species: String)
    case Carrot(length: Double)
    case Onion(layers: Int)
    case Turnip
  object Vegetable:
    given Eq[Vegetable] = Eq.fromUniversalEquals
    given Arbitrary[Vegetable.Potato] = Arbitrary(
      Arbitrary.arbitrary[String].map(Vegetable.Potato.apply)
    )
    given Arbitrary[Vegetable.Carrot] = Arbitrary(
      Arbitrary.arbitrary[Double].map(Vegetable.Carrot.apply)
    )
    given Arbitrary[Vegetable.Onion] = Arbitrary(
      Arbitrary.arbitrary[Int].map(Vegetable.Onion.apply)
    )
    given Arbitrary[Vegetable.Turnip.type] = Arbitrary(Gen.const(Vegetable.Turnip))
    given Arbitrary[Vegetable] = Arbitrary(
      Gen.oneOf(
        Arbitrary.arbitrary[Vegetable.Potato],
        Arbitrary.arbitrary[Vegetable.Carrot],
        Arbitrary.arbitrary[Vegetable.Onion],
        Arbitrary.arbitrary[Vegetable.Turnip.type]
      )
    )

  enum RecursiveEnumAdt derives Codec:
    case BaseAdtExample(a: String)
    case NestedAdtExample(r: RecursiveEnumAdt)
  object RecursiveEnumAdt:
    given Eq[RecursiveEnumAdt] = Eq.fromUniversalEquals

    private def atDepth(depth: Int): Gen[RecursiveEnumAdt] = if (depth < 3)
      Gen.oneOf(
        Arbitrary.arbitrary[String].map(RecursiveEnumAdt.BaseAdtExample(_)),
        atDepth(depth + 1).map(RecursiveEnumAdt.NestedAdtExample(_))
      )
    else Arbitrary.arbitrary[String].map(RecursiveEnumAdt.BaseAdtExample(_))

    given Arbitrary[RecursiveEnumAdt] = Arbitrary(atDepth(0))

  sealed trait ADTWithSubTraitExample derives Codec.AsObject
  sealed trait SubTrait extends ADTWithSubTraitExample
  case class TheClass(a: Int) extends SubTrait

  object ADTWithSubTraitExample:
    given Arbitrary[ADTWithSubTraitExample] = Arbitrary(Arbitrary.arbitrary[Int].map(TheClass.apply))
    given Eq[ADTWithSubTraitExample] = Eq.fromUniversalEquals

  case class ProductWithTaggedMember(x: ProductWithTaggedMember.TaggedString) derives Codec.AsObject

  object ProductWithTaggedMember:
    sealed trait Tag

    type TaggedString = String with Tag

    object TaggedString:
      val decoder: Decoder[TaggedString] =
        summon[Decoder[String]].map(_.asInstanceOf[TaggedString])
      val encoder: Encoder[TaggedString] =
        summon[Encoder[String]].contramap(x => x)

    given Codec[TaggedString] =
      Codec.from(TaggedString.decoder, TaggedString.encoder)

    def fromUntagged(x: String): ProductWithTaggedMember =
      ProductWithTaggedMember(x.asInstanceOf[TaggedString])

    given Arbitrary[ProductWithTaggedMember] =
      Arbitrary {
        Arbitrary.arbitrary[String].map(fromUntagged)
      }
    given Eq[ProductWithTaggedMember] = Eq.fromUniversalEquals

  case class Inner[A](field: A) derives Encoder, Decoder
  case class Outer(a: Option[Inner[String]]) derives Encoder.AsObject, Decoder
  object Outer:
    given Eq[Outer] = Eq.fromUniversalEquals
    given Arbitrary[Outer] =
      Arbitrary(Gen.option(Arbitrary.arbitrary[String].map(Inner.apply)).map(Outer.apply))

  case class LongClass(
    v1: String,
    v2: String,
    v3: String,
    v4: String,
    v5: String,
    v6: String,
    v7: String,
    v8: String,
    v9: String,
    v10: String,
    v11: String,
    v12: String,
    v13: String,
    v14: String,
    v15: String,
    v16: String,
    v17: String,
    v18: String,
    v19: String,
    v20: String,
    v21: String,
    v22: String,
    v23: String,
    v24: String,
    v25: String,
    v26: String,
    v27: String,
    v28: String,
    v29: String,
    v30: String,
    v31: String,
    v32: String,
    v33: String
  ) derives Encoder.AsObject,
        Decoder

  object LongClass:
    given Eq[LongClass] = Eq.fromUniversalEquals
    given Arbitrary[LongClass] = Arbitrary {
      for
        s1 <- Arbitrary.arbitrary[String]
        s2 <- Arbitrary.arbitrary[String]
        s3 <- Arbitrary.arbitrary[String]
        s4 <- Arbitrary.arbitrary[String]
        s5 <- Arbitrary.arbitrary[String]
        s6 <- Arbitrary.arbitrary[String]
        s7 <- Arbitrary.arbitrary[String]
        s8 <- Arbitrary.arbitrary[String]
        s9 <- Arbitrary.arbitrary[String]
        s10 <- Arbitrary.arbitrary[String]
        s11 <- Arbitrary.arbitrary[String]
        s12 <- Arbitrary.arbitrary[String]
        s13 <- Arbitrary.arbitrary[String]
        s14 <- Arbitrary.arbitrary[String]
        s15 <- Arbitrary.arbitrary[String]
        s16 <- Arbitrary.arbitrary[String]
        s17 <- Arbitrary.arbitrary[String]
        s18 <- Arbitrary.arbitrary[String]
        s19 <- Arbitrary.arbitrary[String]
        s20 <- Arbitrary.arbitrary[String]
        s21 <- Arbitrary.arbitrary[String]
        s22 <- Arbitrary.arbitrary[String]
        s23 <- Arbitrary.arbitrary[String]
        s24 <- Arbitrary.arbitrary[String]
        s25 <- Arbitrary.arbitrary[String]
        s26 <- Arbitrary.arbitrary[String]
        s27 <- Arbitrary.arbitrary[String]
        s28 <- Arbitrary.arbitrary[String]
        s29 <- Arbitrary.arbitrary[String]
        s30 <- Arbitrary.arbitrary[String]
        s31 <- Arbitrary.arbitrary[String]
        s32 <- Arbitrary.arbitrary[String]
        s33 <- Arbitrary.arbitrary[String]
      yield LongClass(
        s1,
        s2,
        s3,
        s4,
        s5,
        s6,
        s7,
        s8,
        s9,
        s10,
        s11,
        s12,
        s13,
        s14,
        s15,
        s16,
        s17,
        s18,
        s19,
        s20,
        s21,
        s22,
        s23,
        s24,
        s25,
        s26,
        s27,
        s28,
        s29,
        s30,
        s31,
        s32,
        s33
      )
    }

  enum LongEnum derives Encoder, Decoder:
    case v1, v2, v3, v4, v5, v6, v7, v8, v9, v10,
      v11, v12, v13, v14, v15, v16, v17, v18, v19, v20,
      v21, v22, v23, v24, v25, v26, v27, v28, v29, v30,
      v31, v32, v33

  object LongEnum:
    given Eq[LongEnum] = Eq.fromUniversalEquals
    given Arbitrary[LongEnum] = Arbitrary(Gen.oneOf(LongEnum.values))

  enum LongSum derives Encoder, Decoder:
    case v1(str: String)
    case v2(str: String)
    case v3(str: String)
    case v4(str: String)
    case v5(str: String)
    case v6(str: String)
    case v7(str: String)
    case v8(str: String)
    case v9(str: String)
    case v10(str: String)
    case v11(str: String)
    case v12(str: String)
    case v13(str: String)
    case v14(str: String)
    case v15(str: String)
    case v16(str: String)
    case v17(str: String)
    case v18(str: String)
    case v19(str: String)
    case v20(str: String)
    case v21(str: String)
    case v22(str: String)
    case v23(str: String)
    case v24(str: String)
    case v25(str: String)
    case v26(str: String)
    case v27(str: String)
    case v28(str: String)
    case v29(str: String)
    case v30(str: String)
    case v31(str: String)
    case v32(str: String)
    case v33(str: String)

  object LongSum:
    given Eq[LongSum] = Eq.fromUniversalEquals
    given Arbitrary[LongSum] = Arbitrary(
      for
        v <- Arbitrary.arbitrary[String]
        res <- Gen.oneOf(
          Seq(
            v1(v),
            v2(v),
            v3(v),
            v4(v),
            v5(v),
            v6(v),
            v7(v),
            v8(v),
            v9(v),
            v10(v),
            v11(v),
            v12(v),
            v13(v),
            v14(v),
            v15(v),
            v16(v),
            v17(v),
            v18(v),
            v19(v),
            v20(v),
            v21(v),
            v22(v),
            v23(v),
            v24(v),
            v25(v),
            v26(v),
            v27(v),
            v28(v),
            v29(v),
            v30(v),
            v31(v),
            v32(v),
            v33(v)
          )
        )
      yield res
    )
}

class DerivesSuite extends CirceMunitSuite {
  import DerivesSuite.*
  import io.circe.syntax.*

  checkAll("Codec[Box[Wub]]", CodecTests[Box[Wub]].codec)
  checkAll("Codec[Box[Long]]", CodecTests[Box[Long]].codec)
  // checkAll("Codec[Qux[Long]]", CodecTests[Qux[Long]].codec) Does not compile because Scala 3 requires a `Codec[Long]` for this when you use `derives Codec`
  checkAll("Codec[Seq[Foo]]", CodecTests[Seq[Foo]].codec)
  checkAll("Codec[Baz]", CodecTests[Baz].codec)
  checkAll("Codec[Foo]", CodecTests[Foo].codec)
  checkAll("Codec[RecursiveAdtExample]", CodecTests[RecursiveAdtExample].codec)
  checkAll("Codec[RecursiveWithOptionExample]", CodecTests[RecursiveWithOptionExample].codec)
  checkAll("Codec[Vegetable]", CodecTests[Vegetable].codec)
  checkAll("Codec[RecursiveEnumAdt]", CodecTests[RecursiveEnumAdt].codec)
  checkAll("Codec[ADTWithSubTraitExample]", CodecTests[ADTWithSubTraitExample].codec)
  checkAll("Codec[ProductWithTaggedMember] (#2135)", CodecTests[ProductWithTaggedMember].codec)
  checkAll("Codec[Outer]", CodecTests[Outer].codec)
  checkAll("Codec[LongClass]", CodecTests[LongClass].codec)
  checkAll("Codec[LongSum]", CodecTests[LongSum].codec)
  checkAll("Codec[LongEnum]", CodecTests[LongEnum].codec)

  test("Nested sums should not be encoded redundantly") {
    val foo: ADTWithSubTraitExample = TheClass(0)
    val expected = Json.obj("TheClass" -> Json.obj("a" -> 0.asJson))
    assertEquals(foo.asJson, expected)
  }

  test("Derived Encoder respects existing instances") {
    val some = Outer(Some(Inner("c")))
    val none = Outer(None)
    val expectedSome = Json.obj("a" -> Json.obj("field" -> "c".asJson))
    val expectedNone = Json.obj("a" -> Json.Null)
    assertEquals(some.asJson, expectedSome)
    assertEquals(none.asJson, expectedNone)
  }
}
