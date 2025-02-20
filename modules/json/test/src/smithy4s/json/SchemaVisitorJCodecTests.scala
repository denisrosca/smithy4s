/*
 *  Copyright 2021-2023 Disney Streaming
 *
 *  Licensed under the Tomorrow Open Source Technology License, Version 1.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *     https://disneystreaming.github.io/TOST-1.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package smithy4s
package json

import alloy.Discriminated
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString => _, _}
import munit.FunSuite
import smithy.api.Default
import smithy.api.JsonName
import smithy4s.codecs.PayloadError
import smithy4s.codecs.PayloadPath
import smithy4s.example.CheckedOrUnchecked
import smithy4s.example.CheckedOrUnchecked2
import smithy4s.example.FaceCard
import smithy4s.example.Four
import smithy4s.example.One
import smithy4s.example.PayloadData
import smithy4s.example.RangeCheck
import smithy4s.example.TestBiggerUnion
import smithy4s.example.Three
import smithy4s.example.UntaggedUnion
import smithy4s.example.{OpenEnumTest, OpenIntEnumTest}
import smithy4s.schema.Schema._

import scala.collection.immutable.ListMap
import scala.util.Try

class SchemaVisitorJCodecTests() extends FunSuite {

  case class Foo(a: Int, b: Option[Int])
  object Foo {
    implicit val schema: Schema[Foo] = {
      val a = int.required[Foo]("a", _.a)
      val b = int.optional[Foo]("b", _.b).addHints(JsonName("_b"))
      struct(a, b)(Foo.apply)
    }
  }

  case class FooDefaulted(a: Int)
  object FooDefaulted {
    implicit val schema: Schema[FooDefaulted] = {
      val a =
        int
          .required[FooDefaulted]("a", _.a)
          .addHints(Default(Document.fromInt(11)))
      struct(a)(FooDefaulted.apply)
    }
  }

  private val readerConfig: ReaderConfig = ReaderConfig
    .withThrowReaderExceptionWithStackTrace(true)
    .withAppendHexDumpToParseException(true)
    .withCheckForEndOfInput(false)

  def readFromString[A: JsonCodec](str: String): A = {
    com.github.plokhotnyuk.jsoniter_scala.core
      .readFromString[A](str, readerConfig)
  }

  import Json.deriveJsonCodec

  case class IntList(head: Int, tail: Option[IntList] = None)
  object IntList {
    val hints: smithy4s.Hints = smithy4s.Hints()

    implicit val schema: smithy4s.Schema[IntList] = recursive(
      struct(
        int.required[IntList]("head", _.head),
        IntList.schema.optional[IntList]("tail", _.tail)
      ) {
        IntList.apply
      }
    )
  }

  case class Baz(str: String)
  case class Bin(str: String, int: Int)

  implicit val eitherBazBinSchema: Schema[Either[Baz, Bin]] = {
    val left = struct(string.required[Baz]("str", _.str))(Baz.apply)

    val right = struct(
      string.required[Bin]("str", _.str).addHints(JsonName("binStr")),
      int.required[Bin]("int", _.int)
    )(Bin.apply)
      .addMemberHints(JsonName("binBin"))

    Schema
      .either(left, right)
      .addHints(
        Discriminated("type")
      )
  }

  test(
    "Compiling a codec for a recursive type should not blow up the stack"
  ) {
    val foo = IntList(1, Some(IntList(2)))
    val json = """{"head":1,"tail":{"head":2}}"""
    val result = writeToString[IntList](foo)
    val roundTripped = readFromString[IntList](json)
    expect.same(result, json)
    expect.same(roundTripped, foo)
  }

  test("Optional encode from present value") {
    val foo = Foo(1, Some(2))
    val json = """{"a":1,"_b":2}"""
    val result = writeToString[Foo](foo)
    expect.same(result, json)
  }

  test("Optional decode from present value") {
    val json = """{"a": 1, "_b": 2}"""
    val result = readFromString[Foo](json)
    expect.same(result, Foo(1, Some(2)))
  }

  test("Optional decode from defaulted value - missing") {
    val json = """{}"""
    val result = readFromString[FooDefaulted](json)
    val expected = FooDefaulted(11)
    expect.same(result, expected)
  }

  test("Optional decode from defaulted value - null") {
    val json = """{"a": null}"""
    try {
      readFromString[FooDefaulted](json)
      fail("Expected decoding to fail")
    } catch {
      case e: smithy4s.codecs.PayloadError =>
        expect.same(e.path, PayloadPath.parse(".a"))
    }
  }

  test("Optional decode from absent value") {
    val json = """{"a": 1}"""
    val result = readFromString[Foo](json)
    expect.same(result, Foo(1, None))
  }

  test("Optional decode from null value") {
    val json = """{"a": 1, "_b": null}"""
    val result = readFromString[Foo](json)
    expect.same(result, Foo(1, None))
  }

  test("Optional: path gets surfaced in errors") {
    val json = """{"a": 1, "_b": "foo"}"""
    try {
      val _ = readFromString[Foo](json)
      fail("Unexpected success")
    } catch {
      case PayloadError(path, expected, _) =>
        expect.same(path, PayloadPath("b"))
        expect.same(expected, "JsNull or int")
    }
  }

  test("Required: JsonName is used when missing required field is annotated") {
    val jsonNameValue = "oldName"
    case class Bar(name: String)
    object Bar {
      implicit val schema: Schema[Bar] = {
        val name =
          string.required[Bar]("name", _.name).addHints(JsonName(jsonNameValue))
        struct(name)(Bar.apply)
      }
    }
    val json = """{"missing": "oops"}"""
    try {
      val _ = readFromString[Bar](json)
      fail("Unexpected success")
    } catch {
      case ex @ PayloadError(path, expected, _) =>
        expect(path == PayloadPath(jsonNameValue))
        expect(expected == jsonNameValue)
        expect.same(ex.getMessage(), "Missing required field (path: .oldName)")
    }
  }

  implicit val eitherIntStringSchema: Schema[Either[Int, String]] = {
    Schema.either(int, string.addMemberHints(JsonName("_string")))
  }

  test("Union gets encoded correctly") {
    val jsonInt = """{"left":1}"""
    val jsonStr = """{"_string":"foo"}"""
    val int = writeToString[Either[Int, String]](Left(1))
    val str = writeToString[Either[Int, String]](Right("foo"))
    expect.same(int, jsonInt)
    expect.same(str, jsonStr)
  }

  test("Valid union values are parsed successfuly") {
    val jsonStr = """{"checked":"foo"}"""
    val result = readFromString[CheckedOrUnchecked](jsonStr)
    expect.same(result, CheckedOrUnchecked.CheckedCase("foo"))
  }

  test("Invalid union values fails to parse") {
    val jsonStr = """{"checked":"!@#"}"""
    val result = Try(readFromString[CheckedOrUnchecked](jsonStr)).failed
    expect.same(
      result.get,
      PayloadError(
        PayloadPath.parse(".checked"),
        "string",
        "String '!@#' does not match pattern '^\\w+$'"
      )
    )
  }

  test(
    "Constraints contribute to the discrimination process of untagged union"
  ) {
    val jsonStr = "\"foo\""
    val result = readFromString[CheckedOrUnchecked2](jsonStr)
    expect(result == CheckedOrUnchecked2.CheckedCase("foo"))
    val jsonStr2 = "\"!@#\""
    val result2 = readFromString[CheckedOrUnchecked2](jsonStr2)
    expect.same(result2, CheckedOrUnchecked2.RawCase("!@#"))
  }

  test("Discriminated union gets encoded correctly") {
    val jsonBaz = """{"type":"left","str":"test"}"""
    val jsonBin = """{"type":"binBin","binStr":"foo","int":2022}"""
    val baz = writeToString[Either[Baz, Bin]](Left(Baz("test")))
    val bin = writeToString[Either[Baz, Bin]](Right(Bin("foo", 2022)))
    expect.same(baz, jsonBaz)
    expect.same(bin, jsonBin)
  }

  test("Discriminated union decoding tolerates whitespace") {
    val json = """ { "tpe": "one" , "value": "hello" }"""
    val result = readFromString[TestBiggerUnion](json)

    expect.same(result, TestBiggerUnion.OneCase(One(Some("hello"))))
  }

  test("Discriminated union discriminator can follow other keys") {
    val json = """ { "value": "hello", "tpe": "one" }"""
    val result = readFromString[TestBiggerUnion](json)

    expect.same(result, TestBiggerUnion.OneCase(One(Some("hello"))))
  }

  test("Nested discriminated union decoding tolerates whitespace") {
    val json = """{ "testBiggerUnion": { "tpe": "one", "value": "hello" } }"""
    val result = readFromString[PayloadData](json)

    expect.same(
      result,
      PayloadData(Some(TestBiggerUnion.OneCase(One(Some("hello")))))
    )
  }

  test("Discriminated union gets routed to the correct codec") {
    val jsonBaz = """{"type":"left","str":"test"}"""
    val jsonBin = """{"type":"binBin","binStr":"foo","int":2022}"""
    val baz = readFromString[Either[Baz, Bin]](jsonBaz)
    val bin = readFromString[Either[Baz, Bin]](jsonBin)
    expect.same(baz, Left(Baz("test")))
    expect.same(bin, Right(Bin("foo", 2022)))
  }

  test("Union gets routed to the correct codec") {
    val jsonInt = """{"left":  1}"""
    val jsonStr = """{"_string": "foo"}"""
    val int = readFromString[Either[Int, String]](jsonInt)
    val str = readFromString[Either[Int, String]](jsonStr)
    expect.same(int, Left(1))
    expect.same(str, Right("foo"))
  }

  test("Union: path gets surfaced in errors") {
    val json = """{"left": null}"""
    try {
      val _ = readFromString[Either[Int, String]](json)
      fail("Unexpected success")
    } catch {
      case PayloadError(path, expected, msg) =>
        expect.same(path, PayloadPath("left"))
        expect.same(expected, "int")
        expect(msg.contains("illegal number"))

    }
  }

  test("Union: wrong shape") {
    val json = """null"""
    try {
      val _ = readFromString[Either[Int, String]](json)
      fail("Unexpected success")
    } catch {
      case PayloadError(path, expected, msg) =>
        expect.same(path, PayloadPath.root)
        expect.same(expected, "tagged-union")
        expect(msg.contains("Expected JSON object"))

    }
  }

  test("Untagged union are encoded / decoded") {
    val oneJ = """ {"three":"three_value"}"""
    val twoJ = """ {"four":4}"""
    val oneRes = readFromString[UntaggedUnion](oneJ)
    val twoRes = readFromString[UntaggedUnion](twoJ)

    expect.same(oneRes, UntaggedUnion.ThreeCase(Three("three_value")))
    expect.same(twoRes, UntaggedUnion.FourCase(Four(4)))
  }

  test("Int Enum gets encoded/decoded correctly") {
    val jsonInt = "1"
    val int = writeToString[FaceCard](FaceCard.JACK)
    val roundTripped = readFromString[FaceCard](int)
    expect.same(int, jsonInt)
    expect.same(roundTripped, FaceCard.JACK)
  }

  test("Open Int Enum gets encoded/decoded correctly - known value") {
    val jsonInt = "1"
    val in = writeToString[OpenIntEnumTest](OpenIntEnumTest.ONE)
    val roundTripped = readFromString[OpenIntEnumTest](in)
    expect.same(in, jsonInt)
    expect.same(roundTripped, OpenIntEnumTest.ONE)
  }

  test("Open Int Enum gets encoded/decoded correctly - unknown value") {
    val jsonInt = "123"
    val in = writeToString[OpenIntEnumTest](OpenIntEnumTest.$Unknown(123))
    val roundTripped = readFromString[OpenIntEnumTest](in)
    expect.same(in, jsonInt)
    expect.same(roundTripped, OpenIntEnumTest.$Unknown(123))
  }

  test("Open String Enum gets encoded/decoded correctly - known value") {
    val jsonStr = "\"ONE\""
    val in = writeToString[OpenEnumTest](OpenEnumTest.ONE)
    val roundTripped = readFromString[OpenEnumTest](in)
    expect.same(in, jsonStr)
    expect.same(roundTripped, OpenEnumTest.ONE)
  }

  test("Open String Enum gets encoded/decoded correctly - unknown value") {
    val jsonStr = "\"SOMETHING\""
    val in = writeToString[OpenEnumTest](OpenEnumTest.$Unknown("SOMETHING"))
    val roundTripped = readFromString[OpenEnumTest](in)
    expect.same(in, jsonStr)
    expect.same(roundTripped, OpenEnumTest.$Unknown("SOMETHING"))
  }

  implicit val blobSchema: Schema[Blob] = blob

  test("byte arrays are encoded as base64") {
    val bytes = Blob("foobar")
    val bytesJson = writeToString(bytes)
    val decoded = readFromString[Blob](bytesJson)
    expect.same(bytesJson, "\"Zm9vYmFy\"")
    expect.same(decoded, bytes)
  }

  implicit val documentSchema: Schema[Document] = document
  test("documents get encoded as json") {
    import Document._
    val doc: Document = DObject(
      ListMap(
        "int" -> DNumber(BigDecimal(1)),
        "str" -> DString("hello"),
        "null" -> DNull,
        "bool" -> DBoolean(true),
        "array" -> DArray(IndexedSeq(DString("foo"), DString("bar")))
      )
    )
    val documentJson = writeToString(doc)
    val expected =
      """{"int":1,"str":"hello","null":null,"bool":true,"array":["foo","bar"]}"""

    val decoded = readFromString[Document](documentJson)

    expect.same(documentJson, expected)
    expect.same(decoded, doc)
  }

  test("empty document arrays can be encoded (#1158)") {
    val doc: Document = Document.array()
    val documentJson = writeToString(doc)
    val expected =
      """[]"""

    val decoded = readFromString[Document](documentJson)

    expect.same(documentJson, expected)
    expect.same(decoded, doc)
  }

  test("Range checks are performed correctly") {
    val json = """{"qty":0}"""
    val result = util.Try(readFromString[RangeCheck](json))
    expect(
      result.failed.get.getMessage == "Input must be >= 1.0, but was 0.0 (path: .qty)" ||
        result.failed.get.getMessage == "Input must be >= 1, but was 0 (path: .qty)" // js
    )
  }

  case class Bar(
      str: Option[String],
      lst: Option[List[Int]],
      int: Option[Int]
  )
  object Bar {
    val maxLength = 10
    val lengthHint = smithy.api.Length(max = Some(maxLength.toLong))
    val rangeHint = smithy.api.Range(max = Some(maxLength.toLong))
    implicit val schema: Schema[Bar] = {
      val str = string
        .validated(lengthHint)
        .optional[Bar]("str", _.str)
      val lst = list[Int](int)
        .validated(lengthHint)
        .optional[Bar]("lst", _.lst)
      val intS = int
        .validated(rangeHint)
        .optional[Bar]("int", _.int)
      struct(str, lst, intS)(Bar.apply)
    }
  }

  test("throw PayloadError on String violating length constraint") {
    val str = "a" * (Bar.maxLength + 1)
    val json = s"""{"str":"$str"}"""
    val result = util.Try(readFromString[Bar](json))
    expect.same(
      result.failed.get.getMessage,
      "length required to be <= 10, but was 11 (path: .str)"
    )
  }

  test("throw PayloadError on List violating length constraint") {
    val lst = List.fill(Bar.maxLength + 1)(0)
    val json = s"""{"lst": ${lst.mkString("[", ",", "]")}}"""
    val result = util.Try(readFromString[Bar](json))
    expect.same(
      result.failed.get.getMessage,
      "length required to be <= 10, but was 11 (path: .lst)"
    )
  }

  test("throw PayloadError on Int violating range constraint") {
    val int = Bar.maxLength + 1
    val json = s"""{"int":$int}"""
    val result = util.Try(readFromString[Bar](json))
    expect.same(
      result.failed.get.getMessage,
      (if (!Platform.isJS)
         "Input must be <= 10, but was 11.0 (path: .int)"
       else "Input must be <= 10, but was 11 (path: .int)")
    )
  }

  case class Bar2(str: String)

  case class Foo2(bar: Bar2)
  object Foo2 {
    val maxLength = 10
    val lengthHint = smithy.api.Length(max = Some(maxLength.toLong))
    implicit val schema: Schema[Foo2] = {
      val str = string
        .validated(lengthHint)
        .required[Bar2]("str", _.str)
      val bar = struct(str)(Bar2.apply).required[Foo2]("bar", _.bar)
      struct(bar)(Foo2.apply)
    }
  }

  test(
    "throw PayloadError on Struct[Struct[String]] violating length constraint"
  ) {
    val str = "a" * (Foo2.maxLength + 1)
    val json = s"""{"bar":{"str":"$str"}}"""
    val result = util.Try(readFromString[Foo2](json))
    expect.same(
      result.failed.get.getMessage,
      "length required to be <= 10, but was 11 (path: .bar.str)"
    )
  }

  case class Foo3(bar: List[Bar2])
  object Foo3 {
    val maxLength = 10
    val lengthHint = smithy.api.Length(max = Some(maxLength.toLong))
    implicit val schema: Schema[Foo3] = {
      val str = string
        .validated(lengthHint)
        .required[Bar2]("str", _.str)
      val bar = list(struct(str)(Bar2.apply)).required[Foo3]("bar", _.bar)
      struct(bar)(Foo3.apply)
    }
  }

  test(
    "throw PayloadError on Struct[List[Struct[String]]] violating length constraint"
  ) {
    try {
      val str = "a" * (Foo3.maxLength + 1)
      val json = s"""{"bar":[{"str":"$str"}]}"""
      val _ = readFromString[Foo3](json)
      fail("Unexpected success")
    } catch {
      case PayloadError(path, _, message) =>
        expect.same(message, "length required to be <= 10, but was 11")
        expect.same(path, PayloadPath.parse("bar.0.str"))
    }
  }

  private implicit val schemaMapStringInt: Schema[Map[String, Int]] = {
    map(string, int)
  }

  test("throw PayloadError on Map inserts over maxArity") {
    try {
      val items =
        List.fill(1025)("1").map(i => s""""$i":$i""").mkString("{", ",", "}")
      val _ = readFromString[Map[String, Int]](items)
      fail("Unexpected success")
    } catch {
      case PayloadError(_, _, message) =>
        expect(message == "Input map exceeded max arity of 1024")
    }
  }

  private implicit val schemaVectorInt: Schema[List[Int]] = {
    list(int)
  }

  test("throw PayloadError on Vector inserts over maxArity") {
    try {
      val items = List.fill(1025)("1").mkString("[", ",", "]")
      val _ = readFromString[List[Int]](items)
      fail("Unexpected success")
    } catch {
      case PayloadError(_, _, message) =>
        expect.same(message, "Input list exceeded max arity of 1024")
    }
  }

  test("throw PayloadError on Document list inserts over maxArity") {
    try {
      val items = List.fill(1025)("1").mkString("[", ",", "]")
      val _ = readFromString[Document](items)
      fail("Unexpected success")
    } catch {
      case PayloadError(_, _, message) =>
        expect.same(message, "Input JSON document exceeded max arity of 1024")
    }
  }

  test("throw PayloadError on Document map inserts over maxArity") {
    try {
      val items =
        List.fill(1025)("1").map(i => s""""$i":$i""").mkString("{", ",", "}")
      val _ = readFromString[Document](items)
      fail("Unexpected success")
    } catch {
      case PayloadError(_, _, message) =>
        expect.same(message, "Input JSON document exceeded max arity of 1024")
    }
  }

  test(
    "fields marked with @required and @default should always be encoded"
  ) {
    implicit val requiredFieldSchema: Schema[String] =
      Schema
        .struct[String](
          Schema.string
            .required[String]("test", identity)
            .addHints(smithy.api.Default(Document.fromString("default")))
        )(identity)

    val json = """{"test":"default"}"""
    val result = writeToString[String]("default")
    expect.same(result, json)
  }

  test(
    "fields marked with @default but not @required should be skipped during encoding when matching default"
  ) {
    implicit val fieldSchema: Schema[String] =
      Schema
        .struct[String](
          Schema.string
            .field[String]("test", identity)
            .addHints(smithy.api.Default(Document.fromString("default")))
        )(identity)

    val json = """{}"""
    val result = writeToString[String]("default")
    expect.same(result, json)
  }

}
