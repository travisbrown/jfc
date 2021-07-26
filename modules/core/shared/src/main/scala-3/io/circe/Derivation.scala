package io.circe

import scala.compiletime.constValue
import scala.deriving.Mirror
import Predef.genericArrayOps
import cats.data.{NonEmptyList, Validated}
import io.circe.derivation._

private[circe] trait DerivedInstance[A](
  final val name: String,
  protected[this] final val elemLabels: Array[String]
)

private[circe] trait DerivedEncoder[A](using conf: Configuration) extends DerivedInstance[A] with Encoder.AsObject[A] {
  protected[this] def elemEncoders: Array[Encoder[_]]

  final def encodeWith(index: Int)(value: Any): (String, Json) =
    (elemLabels(index), elemEncoders(index).asInstanceOf[Encoder[Any]].apply(value))
  
  final def encodedIterable(value: Product): Iterable[(String, Json)] =
    new Iterable[(String, Json)]:
      def iterator: Iterator[(String, Json)] =
        value.productIterator.zipWithIndex.map((value, index) => encodeWith(index)(value))
  
  final def encodeProduct(a: A): JsonObject =
    JsonObject.fromIterable(encodedIterable(a.asInstanceOf[Product]))
  
  final def encodeSum(index: Int, a: A): JsonObject = encodeWith(index)(a) match {
    case (k, v) => conf.discriminator match {
      case None => JsonObject.singleton(k, v)
      case Some(discriminator) => v.asObject.getOrElse(JsonObject.empty).add(discriminator, Json.fromString(k))
    }
  }
}

private[circe] trait DerivedDecoder[A](using conf: Configuration) extends DerivedInstance[A] with Decoder[A] {
  protected[this] def elemDecoders: Array[Decoder[_]]
  protected[this] def elemDefaults: Default[A]

  private def decodeSumElement[R](c: HCursor)(fail: DecodingFailure => R, decode: Decoder[A] => ACursor => R): R =
    def fromName(sumTypeName: String, cursor: ACursor): R =
      elemLabels.indexOf(sumTypeName) match {
        case -1 => fail(DecodingFailure(s"type $name hasn't a class/object/case named '$sumTypeName'.", cursor.history))
        case index => decode(elemDecoders(index).asInstanceOf[Decoder[A]])(cursor)
      }
    
    conf.discriminator match {
      case Some(discriminator) =>
        val cursor = c.downField(discriminator)
        cursor.as[Option[String]] match {
          case Left(failure) => fail(failure)
          case Right(None) => fail(DecodingFailure(s"$name: could not find discriminator field '$discriminator' or its null.", cursor.history))
          case Right(Some(sumTypeName)) => fromName(sumTypeName, c)
        }
      case _ =>
        // Should we fail if cursor.keys contains more than one key?
        c.keys.flatMap(_.headOption) match {
          case None => fail(DecodingFailure(name, c.history))
          case Some(sumTypeName) => fromName(sumTypeName, c.downField(sumTypeName))
        }
    }
  final def decodeSum(c: HCursor): Decoder.Result[A] =
    decodeSumElement(c)(Left.apply, _.tryDecode)
  final def decodeSumAccumulating(c: HCursor): Decoder.AccumulatingResult[A] =
    decodeSumElement(c)(Validated.invalidNel, _.tryDecodeAccumulating)
  
  private def decodeProductElement[R](c: HCursor, index: Int, decode: Decoder[Any] => ACursor => R)
    (withDefault: (R, Any) => R): R =
    val decoder = elemDecoders(index).asInstanceOf[Decoder[Any]]
    val field = c.downField(elemLabels(index))
    val result = decode(decoder)(field)
    
    if (conf.useDefaults) {
      elemDefaults.defaultAt(index) match {
        case None => result
        case Some(default) => withDefault(result, default)
      }
    } else {
      result
    }

  final def decodeProduct(c: HCursor, fromProduct: Product => A): Decoder.Result[A] =
    if (c.value.isObject) {
      val res = new Array[Any](elemLabels.length)
      var failed: Left[DecodingFailure, _] = null
      
      var index: Int = 0
      while (index < elemLabels.length && (failed eq null)) {
        decodeProductElement(c, index, _.tryDecode)(withDefault = _ orElse Right(_)) match
          case Right(value) => res(index) = value
          case l @ Left(_) => failed = l
        index += 1
      }
      
      if (failed eq null) {
        Right(fromProduct(Tuple.fromArray(res)))
      } else {
        failed.asInstanceOf[Decoder.Result[A]]
      }
    } else {
      Left(DecodingFailure(name, c.history))
    }
  final def decodeProductAccumulating(c: HCursor, fromProduct: Product => A): Decoder.AccumulatingResult[A] =
    if (c.value.isObject) {
      val res = new Array[Any](elemLabels.length)
      val failed = List.newBuilder[DecodingFailure]
      
      var index: Int = 0
      while (index < elemLabels.length) {
        decodeProductElement(c, index, _.tryDecodeAccumulating)(withDefault = _ orElse Validated.Valid(_)) match {
          case Validated.Valid(value) => res(index) = value
          case Validated.Invalid(failures) => failed ++= failures.toList
        }
        index += 1
      }
      
      val failures = failed.result()
      if (failures.isEmpty) {
        Validated.valid(fromProduct(Tuple.fromArray(res)))
      } else {
        Validated.invalid(NonEmptyList.fromListUnsafe(failures))
      }
    } else {
      Validated.invalidNel(DecodingFailure(name, c.history))
    }
}

private[circe] trait EncoderDerivation {
  inline final def derived[A](using inline A: Mirror.Of[A], conf: Configuration = Configuration.default): Encoder.AsObject[A] =
    new DerivedEncoder[A] with DerivedInstance[A](
      constValue[A.MirroredLabel],
      summonLabels[A.MirroredElemLabels].map(conf.transformNames).toArray
    ) {
      protected[this] lazy val elemEncoders: Array[Encoder[_]] = summonEncoders[A.MirroredElemTypes].toArray
      
      final def encodeObject(a: A): JsonObject = inline A match {
        case m: Mirror.ProductOf[A] => encodeProduct(a)
        case m: Mirror.SumOf[A] => encodeSum(m.ordinal(a), a)
      }
    }
  
  inline final def derive[A: Mirror.Of](
    transformNames: String => String = Configuration.default.transformNames,
    discriminator: Option[String] = Configuration.default.discriminator,
  ): Encoder.AsObject[A] =
    given Configuration = Configuration(transformNames, useDefaults = false, discriminator)
    derived[A]
}

private[circe] trait DecoderDerivation {
  inline final def derived[A](using inline A: Mirror.Of[A], conf: Configuration = Configuration.default): Decoder[A] =
    new DerivedDecoder[A] with DerivedInstance[A](
      constValue[A.MirroredLabel],
      summonLabels[A.MirroredElemLabels].map(conf.transformNames).toArray,
    ):
      protected[this] lazy val elemDecoders: Array[Decoder[_]] = summonDecoders[A.MirroredElemTypes].toArray
      protected[this] lazy val elemDefaults: Default[A] = Predef.summon[Default[A]]
      
      final def apply(c: HCursor): Decoder.Result[A] =
        inline A match {
          case m: Mirror.ProductOf[A] => decodeProduct(c, m.fromProduct)
          case m: Mirror.SumOf[A] => decodeSum(c)
        }
      final override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] =
        inline A match {
          case m: Mirror.ProductOf[A] => decodeProductAccumulating(c, m.fromProduct)
          case m: Mirror.SumOf[A] => decodeSumAccumulating(c)
        }
  
  inline final def derive[A: Mirror.Of](
    transformNames: String => String = Configuration.default.transformNames,
    useDefaults: Boolean = Configuration.default.useDefaults,
    discriminator: Option[String] = Configuration.default.discriminator,
  ): Decoder[A] =
    given Configuration = Configuration(transformNames, useDefaults, discriminator)
    derived[A]
}

private[circe] trait CodecDerivation {
  inline final def derived[A](using inline A: Mirror.Of[A], conf: Configuration = Configuration.default): Codec.AsObject[A] =
    new Codec.AsObject[A]
        with DerivedDecoder[A]
        with DerivedEncoder[A]
        with DerivedInstance[A](
          constValue[A.MirroredLabel],
          summonLabels[A.MirroredElemLabels].map(conf.transformNames).toArray,
        ) {
      protected[this] lazy val elemEncoders: Array[Encoder[_]] = summonEncoders[A.MirroredElemTypes].toArray
      protected[this] lazy val elemDecoders: Array[Decoder[_]] = summonDecoders[A.MirroredElemTypes].toArray
      protected[this] lazy val elemDefaults: Default[A] = Predef.summon[Default[A]]

      final def encodeObject(a: A): JsonObject = inline A match {
        case m: Mirror.ProductOf[A] => encodeProduct(a)
        case m: Mirror.SumOf[A] => encodeSum(m.ordinal(a), a)
      }
      
      final def apply(c: HCursor): Decoder.Result[A] =
        inline A match {
          case m: Mirror.ProductOf[A] => decodeProduct(c, m.fromProduct)
          case m: Mirror.SumOf[A] => decodeSum(c)
        }
      
      final override def decodeAccumulating(c: HCursor): Decoder.AccumulatingResult[A] =
        inline A match {
          case m: Mirror.ProductOf[A] => decodeProductAccumulating(c, m.fromProduct)
          case m: Mirror.SumOf[A] => decodeSumAccumulating(c)
        }
  }

  inline final def derive[A: Mirror.Of](
    transformNames: String => String = Configuration.default.transformNames,
    useDefaults: Boolean = Configuration.default.useDefaults,
    discriminator: Option[String] = Configuration.default.discriminator,
  ): Codec.AsObject[A] =
    given Configuration = Configuration(transformNames, useDefaults, discriminator)
    derived[A]
}
