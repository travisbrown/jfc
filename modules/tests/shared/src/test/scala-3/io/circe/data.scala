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

import io.circe.derivation.*
import cats.kernel.Eq
import cats.kernel.instances.all.*
import cats.syntax.eq.*
import io.circe.testing.CodecTests
import io.circe.tests.CirceMunitSuite
import org.scalacheck.{ Arbitrary, Gen }

object data {

  object withDropNoneValues {

    given Configuration = Configuration.default.withDropNoneValues(true)

    case class AnotherBox[A](a: A) derives Decoder, Encoder

    object AnotherBox {
      implicit def eqBox[A: Eq]: Eq[AnotherBox[A]] = Eq.by(_.a)

      implicit def arbitraryBox[A](implicit A: Arbitrary[A]): Arbitrary[AnotherBox[A]] = Arbitrary(
        A.arbitrary.map(AnotherBox(_))
      )
    }

    case class WithNullables(
      a: String,
      b: NullOr[String],
      c: Option[NullOr[Int]],
      d: NullOr[Boolean],
      e: NullOr[AnotherBox[String]],
      f: NullOr[List[String]],
      g: Option[AnotherBox[String]]
    ) derives ConfiguredDecoder,
          ConfiguredEncoder

    object WithNullables {
      given Eq[WithNullables] = Eq.fromUniversalEquals

      given Arbitrary[WithNullables] = {
        given aNullable[A](using A: Arbitrary[A]): Arbitrary[NullOr[A]] =
          Arbitrary[NullOr[A]](
            A.arbitrary.flatMap { a =>
              summon[Arbitrary[Int]].arbitrary.map {
                case byte if byte % 3 == 0 =>
                  NullOr.Null: NullOr[A]
                case _ =>
                  NullOr.Value(a): NullOr[A]
              }
            }
          )

        val gen = for {
          a <- summon[Arbitrary[String]].arbitrary
          b <- summon[Arbitrary[NullOr[String]]].arbitrary
          c <- summon[Arbitrary[Option[NullOr[Int]]]].arbitrary
          d <- summon[Arbitrary[NullOr[Boolean]]].arbitrary
          e <- summon[Arbitrary[NullOr[AnotherBox[String]]]].arbitrary
          f <- summon[Arbitrary[NullOr[List[String]]]].arbitrary
          g <- summon[Arbitrary[Option[AnotherBox[String]]]].arbitrary
        } yield WithNullables(a, b, c, d, e, f, g)
        Arbitrary(gen)
      }

    }

  }

}
