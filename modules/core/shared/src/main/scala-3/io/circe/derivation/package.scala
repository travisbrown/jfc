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

package io.circe.derivation

import scala.compiletime.{ codeOf, constValue, erasedValue, error, summonFrom, summonInline }
import scala.deriving.Mirror
import io.circe.{ Codec, Decoder, Encoder }

private[circe] inline final def summonLabels[T <: Tuple]: List[String] =
  loopUnrolled[String, T](constString)

@deprecated("Use summonEncoders(derivingForSum: Boolean) instead", "0.14.7")
private[circe] inline final def summonEncoders[T <: Tuple](using Configuration): List[Encoder[_]] =
  summonEncoders(false)

private[circe] inline final def summonEncoders[T <: Tuple](inline derivingForSum: Boolean)(using
  Configuration
): List[Encoder[_]] =
  loopUnrolled[Encoder[_], T](
    inline if (derivingForSum) new EncoderDeriveSum else new EncoderNotDeriveSum
  )

@deprecated("Use summonEncoder(derivingForSum: Boolean) instead", "0.14.7")
private[circe] inline final def summonEncoder[A](using Configuration): Encoder[A] =
  summonEncoder(false)

private[circe] inline final def summonEncoder[A](inline derivingForSum: Boolean)(using Configuration): Encoder[A] =
  summonFrom {
    case encodeA: Encoder[A] => encodeA
    case m: Mirror.Of[A] =>
      inline if (derivingForSum) ConfiguredEncoder.derived[A]
      else error("Failed to find an instance of Encoder[" + typeName[A] + "]")
  }

@deprecated("Use summonDecoders(derivingForSum: Boolean) instead", "0.14.7")
private[circe] inline final def summonDecoders[T <: Tuple](using Configuration): List[Decoder[_]] =
  summonDecoders(false)

private[circe] inline final def summonDecoders[T <: Tuple](inline derivingForSum: Boolean)(using
  Configuration
): List[Decoder[_]] =
  loopUnrolled[Decoder[_], T](
    inline if (derivingForSum) new DecoderDeriveSum else new DecoderNotDeriveSum
  )

@deprecated("Use summonDecoder(derivingForSum: Boolean) instead", "0.14.7")
private[circe] inline final def summonDecoder[A](using Configuration): Decoder[A] =
  summonDecoder(false)

private[circe] inline final def summonDecoder[A](inline derivingForSum: Boolean)(using Configuration): Decoder[A] =
  summonFrom {
    case decodeA: Decoder[A] => decodeA
    case m: Mirror.Of[A] =>
      inline if (derivingForSum) ConfiguredDecoder.derived[A]
      else error("Failed to find an instance of Decoder[" + typeName[A] + "]")
  }

private[circe] inline def summonSingletonCases[T <: Tuple, A](inline typeName: Any): List[A] =
  loopUnrolled[A, T](new SummonSingleton[A](typeName))

private[circe] inline final def loopUnrolled[A, T <: Tuple](f: Inliner[A]): List[A] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (h1 *: h2 *: h3 *: h4 *: h5 *: h6 *: h7 *: h8 *: h9 *: h10 *: h11 *: h12 *: h13 *: h14 *: h15 *: h16 *:
          ts) =>
      f[h1] :: f[h2] :: f[h3] :: f[h4] :: f[h5] :: f[h6] :: f[h7] :: f[h8]
        :: f[h9] :: f[h10] :: f[h11] :: f[h12] :: f[h13] :: f[h14] :: f[h15] :: f[h16]
        :: loopUnrolled[A, ts](f)
    case _: (h1 *: h2 *: h3 *: h4 *: ts) =>
      f[h1] :: f[h2] :: f[h3] :: f[h4] :: loopUnrolled[A, ts](f)
    case _: (h *: ts) => f[h] :: loopUnrolled[A, ts](f)

private[circe] abstract class Inliner[A]:
  inline def apply[T]: A

private[circe] object constString extends Inliner[String]:
  inline def apply[T]: String = constValue[T].asInstanceOf[String]

private[circe] class EncoderDeriveSum(using config: Configuration) extends Inliner[Encoder[?]]:
  inline def apply[T]: Encoder[?] = summonEncoder[T](true)

private[circe] class EncoderNotDeriveSum(using config: Configuration) extends Inliner[Encoder[?]]:
  inline def apply[T]: Encoder[?] = summonEncoder[T](false)

private[circe] class DecoderDeriveSum(using Configuration) extends Inliner[Decoder[?]]:
  inline def apply[T]: Decoder[?] = summonDecoder[T](true)

private[circe] class DecoderNotDeriveSum(using Configuration) extends Inliner[Decoder[?]]:
  inline def apply[T]: Decoder[?] = summonDecoder[T](false)

private[circe] class SummonSingleton[A](typeName: Any) extends Inliner[A]:
  inline def apply[T]: A =
    inline summonInline[Mirror.Of[T]] match
      case m: Mirror.Singleton => m.fromProduct(EmptyTuple).asInstanceOf[A]
      case m: Mirror =>
        error("Enum " + codeOf(typeName) + " contains non singleton case " + codeOf(constValue[m.MirroredLabel]))
