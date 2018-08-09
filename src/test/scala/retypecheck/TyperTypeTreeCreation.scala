package retypecheck

import org.scalatest._
import scala.language.existentials

class TyperTypeTreeCreationSpec extends FlatSpec with Matchers {
  behavior of "Typer for type tree creation"

  class Test { type Type }

  object Test { type Type }

  it should "generate correct type trees" in {
    TyperTester.createTypeTree[Test.Type] should be (
      "TyperTypeTreeCreationSpec.this.Test.Type")
    TyperTester.createTypeTree[Test.type#Type] should be (
      "TyperTypeTreeCreationSpec.this.Test.Type")
    TyperTester.createTypeTree[Test#Type] should be (
      "TyperTypeTreeCreationSpec.this.Test#Type")

    TyperTester.createTypeTree[String] should be (
      "_root_.scala.Predef.String")
    TyperTester.createTypeTree[List[Int]] should be (
      "_root_.scala.List[_root_.scala.Int]")
    TyperTester.createTypeTree[List[_]] should be (
      "_root_.scala.List[_$1] forSome { type _$1 }")
    TyperTester.createTypeTree[List[_ <: Test]] should be (
      "_root_.scala.List[_$2] forSome { type _$2 <: TyperTypeTreeCreationSpec.this.Test }")
    TyperTester.createTypeTree[List[_ <: Test { type T; val v: Int; def d: String }]] should be (
      "_root_.scala.List[_$3] forSome { type _$3 <: TyperTypeTreeCreationSpec.this.Test { type T; val v: _root_.scala.Int; def d: _root_.scala.Predef.String } }")

    TyperTester.createTypeTree[(T, T) forSome { type T }] should be (
      "_root_.scala.Tuple2[T, T] forSome { type T }")
    TyperTester.createTypeTree[(T, T) forSome { type T <: Test }] should be (
      "_root_.scala.Tuple2[T, T] forSome { type T <: TyperTypeTreeCreationSpec.this.Test }")
    TyperTester.createTypeTree[(T, T) forSome { type T <: Test#Type { type T } }] should be (
      "_root_.scala.Tuple2[T, T] forSome { type T <: TyperTypeTreeCreationSpec.this.Test#Type { type T } }")
  }
}
