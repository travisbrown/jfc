Quick Start
===========

circe is published to [Maven Central][maven-central] and cross-built for Scala 2.12, 2.13 and 3.x,
so you can just add the following to your build:

```scala
val circeVersion = "@VERSION@"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
```

In case of large or deep-nested case classes, there is a chance to get stack overflow during compilation,
please refer to [known-issues](codecs/known-issues.md) for workaround.

If you're using the `@JsonCodec` macro annotations in circe's `generic-extras` module,
you'll need to add `-Ymacro-annotations` to your compiler options on Scala 2.13,
or include the [Macro Paradise][paradise] compiler plugin in your build on
earlier Scala versions:

```scala
addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
)
```

Then type `sbt console` to start a REPL and then paste the following (this will also work from the
root directory of this repository):

```scala
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

sealed trait Foo
case class Bar(xs: Vector[String]) extends Foo
case class Qux(i: Int, d: Option[Double]) extends Foo

val foo: Foo = Qux(13, Some(14.0))

val json = foo.asJson.noSpaces
println(json)

val decodedFoo = decode[Foo](json)
println(decodedFoo)
```

Alternatively you can experiment with circe directly in your browser by clicking the `Run` button in the code block and
making modifications in the code.

No boilerplate, no runtime reflection.
