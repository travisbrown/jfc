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

import cats.data.NonEmptyList
import cats.data.Validated
import io.circe.DecodingFailure.Reason.WrongTypeExpectation

import scala.collection.mutable.Builder
import io.circe.CursorOp.MoveRight
import io.circe.CursorOp.DownArray
import io.circe.cursor.ArrayCursor

private[circe] abstract class SeqDecoder[A, C[_]](decodeA: Decoder[A]) extends Decoder[C[A]] {
  protected def createBuilder(): Builder[A, C[A]]

  def apply(c: HCursor): Decoder.Result[C[A]] = {
    val values = c.value.asArray
    if (values.nonEmpty) {
      val jsonValues = values.get
      val builder = createBuilder()
      builder.sizeHint(jsonValues.size)
      var failed: DecodingFailure = null
      var index = 0
      var cursor: ACursor = new ArrayCursor(jsonValues, index, c, false)(c, CursorOp.DownArray)

      while (failed.eq(null) && index < jsonValues.size) {
        cursor.as(decodeA) match {
          case Left(e) =>
            failed = e.copy(history = e.history).withReason(e.reason)
          case Right(a) =>
            builder += a
            cursor = cursor.right
        }
        index += 1
      }

      if (failed.eq(null)) Right(builder.result()) else Left(failed)
    } else {
      if (c.value.isArray) Right(createBuilder().result())
      else {
        Left(DecodingFailure(WrongTypeExpectation("array", c.value), c.history))
      }
    }
  }

  override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[C[A]] = {
    val values = c.value.asArray
    if (values.nonEmpty) {
      val jsonValues = values.get
      val builder = createBuilder()
      builder.sizeHint(jsonValues.size)
      var failed = false
      val failures = List.newBuilder[DecodingFailure]
      var index = 0
      var cursor = c.downArray

      while (index < jsonValues.size) {
        decodeA.decodeAccumulating(cursor.asInstanceOf[HCursor]) match {
          case Validated.Invalid(es) =>
            failed = true
            val fixedErrors = es.map { e =>
              e.copy(history = e.history).withReason(e.reason)
            }
            failures += fixedErrors.head
            failures ++= fixedErrors.tail
          case Validated.Valid(a) =>
            if (!failed) {
              builder += a
            }
        }
        index += 1
        cursor = cursor.right
      }

      if (!failed) Validated.valid(builder.result())
      else {
        failures.result() match {
          case h :: t => Validated.invalid(NonEmptyList(h, t))
          case Nil    => Validated.valid(builder.result())
        }
      }
    } else {
      if (c.value.isArray) Validated.valid(createBuilder().result())
      else {
        Validated.invalidNel(DecodingFailure(WrongTypeExpectation("array", c.value), c.history))
      }
    }
  }
}
