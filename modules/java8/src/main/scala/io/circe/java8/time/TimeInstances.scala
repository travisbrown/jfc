package io.circe.java8.time

import io.circe.{ Decoder, DecodingFailure, Encoder, Json }
import io.circe.java8.time.TimeInstances._
import java.time.{
  DateTimeException,
  Duration,
  Instant,
  LocalDate,
  LocalDateTime,
  LocalTime,
  OffsetDateTime,
  OffsetTime,
  Period,
  YearMonth,
  ZonedDateTime,
  ZoneId
}
import java.time.format.{ DateTimeFormatter, DateTimeParseException }
import java.time.format.DateTimeFormatter.{
  ISO_LOCAL_DATE,
  ISO_LOCAL_DATE_TIME,
  ISO_LOCAL_TIME,
  ISO_OFFSET_DATE_TIME,
  ISO_OFFSET_TIME,
  ISO_ZONED_DATE_TIME
}

trait TimeInstances {
  implicit final val decodeInstant: Decoder[Instant] =
    Decoder.instance { c =>
      c.as[String] match {
        case Right(s) => try Right(Instant.parse(s)) catch {
          case exc: DateTimeParseException => Left(DecodingFailure(formatMessage("Instant", exc), c.history))
        }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[Instant]]
      }
    }

  implicit final val encodeInstant: Encoder[Instant] = Encoder.instance(time => Json.fromString(time.toString))

  implicit final val decodeZoneId: Decoder[ZoneId] =
    Decoder.instance{ c =>
      c.as[String] match {
        case Right(s) =>
          try Right(ZoneId.of(s)) catch {
            case exc: DateTimeException => Left(DecodingFailure(formatZoneIdMessage(exc), c.history))
          }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[ZoneId]]
      }
    }

  implicit final val encodeZoneId: Encoder[ZoneId] =
    Encoder[String].contramap(_.getId)

  final def decodeLocalDateTime(formatter: DateTimeFormatter): Decoder[LocalDateTime] =
    Decoder.instance { c =>
      c.as[String] match {
        case Right(s) => try Right(LocalDateTime.parse(s, formatter)) catch {
          case exc: DateTimeParseException => Left(DecodingFailure(formatMessage("LocalDateTime", exc), c.history))
        }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[LocalDateTime]]
      }
    }

  final def encodeLocalDateTime(formatter: DateTimeFormatter): Encoder[LocalDateTime] =
    Encoder.instance(time => Json.fromString(time.format(formatter)))

  implicit final val decodeLocalDateTimeDefault: Decoder[LocalDateTime] = decodeLocalDateTime(ISO_LOCAL_DATE_TIME)
  implicit final val encodeLocalDateTimeDefault: Encoder[LocalDateTime] = encodeLocalDateTime(ISO_LOCAL_DATE_TIME)

  final def decodeZonedDateTime(formatter: DateTimeFormatter): Decoder[ZonedDateTime] =
    Decoder.instance { c =>
      c.as[String] match {
        case Right(s) => try Right(ZonedDateTime.parse(s, formatter)) catch {
          case exc: DateTimeParseException => Left(DecodingFailure(formatMessage("ZonedDateTime", exc), c.history))
        }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[ZonedDateTime]]
      }
    }

  final def encodeZonedDateTime(formatter: DateTimeFormatter): Encoder[ZonedDateTime] =
    Encoder.instance(time => Json.fromString(time.format(formatter)))

  implicit final val decodeZonedDateTimeDefault: Decoder[ZonedDateTime] = decodeZonedDateTime(ISO_ZONED_DATE_TIME)
  implicit final val encodeZonedDateTimeDefault: Encoder[ZonedDateTime] = encodeZonedDateTime(ISO_ZONED_DATE_TIME)

  final def decodeOffsetDateTime(formatter: DateTimeFormatter): Decoder[OffsetDateTime] =
    Decoder.instance { c =>
      c.as[String] match {
        case Right(s) => try Right(OffsetDateTime.parse(s, formatter)) catch {
          case exc: DateTimeParseException => Left(DecodingFailure(formatMessage("OffsetDateTime", exc), c.history))
        }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[OffsetDateTime]]
      }
    }

  final def encodeOffsetDateTime(formatter: DateTimeFormatter): Encoder[OffsetDateTime] =
    Encoder.instance(time => Json.fromString(time.format(formatter)))

  implicit final val decodeOffsetDateTimeDefault: Decoder[OffsetDateTime] = decodeOffsetDateTime(ISO_OFFSET_DATE_TIME)
  implicit final val encodeOffsetDateTimeDefault: Encoder[OffsetDateTime] = encodeOffsetDateTime(ISO_OFFSET_DATE_TIME)

  final def decodeLocalDate(formatter: DateTimeFormatter): Decoder[LocalDate] =
    Decoder.instance { c =>
      c.as[String] match {
        case Right(s) => try Right(LocalDate.parse(s, formatter)) catch {
          case exc: DateTimeParseException => Left(DecodingFailure(formatMessage("LocalDate", exc), c.history))
        }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[LocalDate]]
      }
    }

  final def encodeLocalDate(formatter: DateTimeFormatter): Encoder[LocalDate] =
    Encoder.instance(time => Json.fromString(time.format(formatter)))

  implicit final val decodeLocalDateDefault: Decoder[LocalDate] = decodeLocalDate(ISO_LOCAL_DATE)
  implicit final val encodeLocalDateDefault: Encoder[LocalDate] = encodeLocalDate(ISO_LOCAL_DATE)

  final def decodeLocalTime(formatter: DateTimeFormatter): Decoder[LocalTime] =
    Decoder.instance { c =>
      c.as[String] match {
        case Right(s) => try Right(LocalTime.parse(s, formatter)) catch {
          case exc: DateTimeParseException => Left(DecodingFailure(formatMessage("LocalTime", exc), c.history))
        }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[LocalTime]]
      }
    }

  final def encodeLocalTime(formatter: DateTimeFormatter): Encoder[LocalTime] =
    Encoder.instance(time => Json.fromString(time.format(formatter)))

  implicit final val decodeLocalTimeDefault: Decoder[LocalTime] = decodeLocalTime(ISO_LOCAL_TIME)
  implicit final val encodeLocalTimeDefault: Encoder[LocalTime] = encodeLocalTime(ISO_LOCAL_TIME)

  final def decodeOffsetTime(formatter: DateTimeFormatter): Decoder[OffsetTime] =
    Decoder.instance { c =>
      c.as[String] match {
        case Right(s) => try Right(OffsetTime.parse(s, formatter)) catch {
          case exc: DateTimeParseException => Left(DecodingFailure(formatMessage("OffsetTime", exc), c.history))
        }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[OffsetTime]]
      }
    }

  final def encodeOffsetTime(formatter: DateTimeFormatter): Encoder[OffsetTime] =
    Encoder.instance(time => Json.fromString(time.format(formatter)))

  implicit final val decodeOffsetTimeDefault: Decoder[OffsetTime] = decodeOffsetTime(ISO_OFFSET_TIME)
  implicit final val encodeOffsetTimeDefault: Encoder[OffsetTime] = encodeOffsetTime(ISO_OFFSET_TIME)

  implicit final val decodePeriod: Decoder[Period] = Decoder.instance { c =>
    c.as[String] match {
      case Right(s) => try Right(Period.parse(s)) catch {
        case _: DateTimeParseException =>
          // For some reason the error message for Duration does not contain the input String by default
          Left(DecodingFailure(s"Period (Text '$s' cannot be parsed to a Period)", c.history))
      }
      case l@Left(_) => l.asInstanceOf[Decoder.Result[Period]]
    }
  }

  implicit final val encodePeriod: Encoder[Period] = Encoder.instance { period =>
    Json.fromString(period.toString)
  }

  final def decodeYearMonth(formatter: DateTimeFormatter): Decoder[YearMonth] =
    Decoder.instance { c =>
      c.as[String] match {
        case Right(s) =>
          try Right(YearMonth.parse(s, formatter))
          catch {
            case exc: DateTimeParseException => Left(DecodingFailure(formatMessage("YearMonth", exc), c.history))
          }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[YearMonth]]
      }
    }

  final def encodeYearMonth(formatter: DateTimeFormatter): Encoder[YearMonth] =
    Encoder.instance(time => Json.fromString(time.format(formatter)))

  private final val yearMonthFormatter = DateTimeFormatter.ofPattern("yyyy-MM")

  implicit final val decodeYearMonthDefault: Decoder[YearMonth] = decodeYearMonth(yearMonthFormatter)
  implicit final val encodeYearMonthDefault: Encoder[YearMonth] = encodeYearMonth(yearMonthFormatter)

  implicit final val decodeDuration: Decoder[Duration] =
    Decoder.instance { c =>
      c.as[String] match {
        case Right(s) => try Right(Duration.parse(s)) catch {
          case _: DateTimeParseException =>
            // For some reason the error message for Duration does not contain the input String by default
            Left(DecodingFailure(s"Duration (Text '$s' cannot be parsed to a Duration)", c.history))
        }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[Duration]]
      }
    }

  implicit final val encodeDuration: Encoder[Duration] =
    Encoder.instance(duration => Json.fromString(duration.toString))
}

/**
  * The functions here need to be in the companion object instead of the trait above so
  * Java serialization doesn't crash.
  */
private[time] object TimeInstances {

  /**
    * Adds error information of `DateTimeParseException` to the error message of `DecodingFailure`.
    */
  private final def formatMessage(typeName: String, dateTimeParseException: DateTimeParseException): String =
    Option(dateTimeParseException.getMessage).fold(typeName) { errMsg =>
      s"$typeName (DateTimeParseException: $errMsg)"
    }

  /**
    * Adds error information of `DateTimeException` to the error message of `DecodingFailure` for `ZoneId`.
    */
  private final def formatZoneIdMessage(dateTimeException: DateTimeException): String =
    Option(dateTimeException.getMessage).fold("ZoneId") { errMsg =>
      s"ZoneId (DateTimeException: $errMsg)"
    }
}
