package retypecheck

import org.scalatest._
import globalObject._

object globalObject {
  def globalValue = 3
}

class TyperSpec extends FlatSpec with Matchers {
  class A(val i: Int)
  case class B(j: Int) extends A(0)

  def markUsed(v: => Any) = ()

  behavior of "Typer"

  it should "typecheck case classes" in {
    "TyperTester.retyper { case class A(); val x = A() }" should compile
    "TyperTester.retyperAll { case class A(); val x = A() }" should compile
    "@TyperTester.retyper class C { case class A(); val x = A() }" should compile
    "@TyperTester.retyperAll class C { case class A(); val x = A() }" should compile
  }

  it should "typecheck case objects" in {
    "TyperTester.retyper { case object A; val x = A }" should compile
    "TyperTester.retyperAll { case object A; val x = A }" should compile
    "@TyperTester.retyper class C { case object A; val x = A }" should compile
    "@TyperTester.retyperAll class C { case object A; val x = A }" should compile
  }

  it should "typecheck vals" in {
    "TyperTester.retyper { val a = 0; val x = a }" should compile
    "TyperTester.retyperAll { val a = 0; val x = a }" should compile
    "@TyperTester.retyper class C { val a = 0; val x = a }" should compile
    "@TyperTester.retyperAll class C { val a = 0; val x = a }" should compile
  }

  it should "typecheck vars" in {
    "TyperTester.retyper { var a = 0; val x = a; a = 0 }" should compile
    "TyperTester.retyperAll { var a = 0; val x = a; a = 0 }" should compile
    "@TyperTester.retyper class C { var a = 0; val x = a; a = 0 }" should compile
    "@TyperTester.retyperAll class C { var a = 0; val x = a; a = 0 }" should compile
  }

  it should "typecheck lazy vals" in {
    "TyperTester.retyper { lazy val a = 0; val x = a }" should compile
    "TyperTester.retyperAll { lazy val a = 0; val x = a }" should compile
    "@TyperTester.retyper class C { lazy val a = 0; val x = a }" should compile
    "@TyperTester.retyperAll class C { lazy val a = 0; val x = a }" should compile
  }

  it should "typecheck getter/setter properties" in {
    "TyperTester.retyper { def a = 0; def a_=(v: Int) = (); val x = a; a = 0 }" shouldNot typeCheck
    "TyperTester.retyperAll { def a = 0; def a_=(v: Int) = (); val x = a; a = 0 }" shouldNot typeCheck
    "@TyperTester.retyper class C { def a = 0; def a_=(v: Int) = (); val x = a; a = 0 }" should compile
    "@TyperTester.retyperAll class C { def a = 0; def a_=(v: Int) = (); val x = a; a = 0 }" should compile
  }

  it should "typecheck compiler-generated class tags" in {
    "TyperTester.retyper { scala.reflect.classTag[A] }" should compile
    "TyperTester.retyperAll { scala.reflect.classTag[A] }" should compile
    "@TyperTester.retyper class C { scala.reflect.classTag[A] }" should compile
    "@TyperTester.retyperAll class C { scala.reflect.classTag[A] }" should compile
  }

  it should "typecheck nested classes of the same name" in {
    "TyperTester.retyper { class C { class C } }" should compile
    "TyperTester.retyperAll { class C { class C } }" should compile
    "@TyperTester.retyper class C { class C { class C } }" should compile
    "@TyperTester.retyperAll class C { class C { class C } }" should compile
  }

  it should "typecheck extractors" in {
    """
    TyperTester.retyper {
      List.empty[Int] match { case List(_, v) => v case _ => 0 }
    }
    """ should compile

    """
    TyperTester.retyperAll {
      List.empty[Int] match { case List(_, v) => v case _ => 0 }
    }
    """ should compile

    """
    @TyperTester.retyper class C {
      List.empty[Int] match { case List(_, v) => v case _ => 0 }
    }
    """ should compile

    """
    @TyperTester.retyperAll class C {
      List.empty[Int] match { case List(_, v) => v case _ => 0 }
    }
    """ should compile
  }

  it should "typecheck renamed imports" in {
    import scala.collection.immutable.{ Set => ImmutableSet }

    "TyperTester.retyper { ImmutableSet(1, 2, 3) }" should compile
    "TyperTester.retyperAll { ImmutableSet(1, 2, 3) }" should compile
    "@TyperTester.retyper class C { ImmutableSet(1, 2, 3) }" should compile
    "@TyperTester.retyperAll class C { ImmutableSet(1, 2, 3) }" should compile

    "TyperTester.retyper { null.asInstanceOf[ImmutableSet[Int]] }" should compile
    "TyperTester.retyperAll { null.asInstanceOf[ImmutableSet[Int]] }" should compile
    "@TyperTester.retyper class C { null.asInstanceOf[ImmutableSet[Int]] }" should compile
    "@TyperTester.retyperAll class C { null.asInstanceOf[ImmutableSet[Int]] }" should compile
  }

  it should "typecheck outer locally singleton type parameters" in {
    class A[T]
    implicit val c = { val s = ""; new A[s.type] }
    def test[T](implicit c: A[T]): A[T] = { markUsed(c); ??? }

    markUsed(test)

    "TyperTester.retyper { val v = test; val w: A[_ <: String with Singleton] = v }" should compile
    "TyperTester.retyperAll { val v = test; val w: A[_ <: String with Singleton] = v }" should compile
    "@TyperTester.retyper class C { val v = test; val w: A[_ <: String with Singleton] = v }" should compile
    "@TyperTester.retyperAll class C { val v = test; val w: A[_ <: String with Singleton] = v }" should compile
  }

  it should "typecheck outer locally path-dependent singleton type parameters" in {
    class X[T]
    class Y
    class Z extends Y
    implicit def c = new Z
    def test(implicit c: Y): X[c.type] = { markUsed(c); ??? }

    markUsed(test)

    "TyperTester.retyper { val v = test; val w: X[_ <: Z with Singleton] = v }" should compile
    "TyperTester.retyperAll { val v = test; val w: X[_ <: Z with Singleton] = v }" should compile
    "@TyperTester.retyper class C { val v = test; val w: X[_ <: Z with Singleton] = v }" should compile
    "@TyperTester.retyperAll class C { val v = test; val w: X[_ <: Z with Singleton] = v }" should compile
  }

  it should "typecheck inner locally singleton type parameters" in {
    """TyperTester.retyper {
      class A[T]
      implicit val c = { val s = ""; new A[s.type] }
      def test[T](implicit c: A[T]): A[T] = ???

      val v = test
      val w: A[_ <: String with Singleton] = v
    }""" should compile

    """TyperTester.retyperAll {
      class A[T]
      implicit val c = { val s = ""; new A[s.type] }
      def test[T](implicit c: A[T]): A[T] = ???

      val v = test
      val w: A[_ <: String with Singleton] = v
    }""" should compile

    """@TyperTester.retyper class C {
      class A[T]
      implicit val c = { val s = ""; new A[s.type] }
      def test[T](implicit c: A[T]): A[T] = ???

      val v = test
      val w: A[_ <: String with Singleton] = v
    }""" should compile

    """@TyperTester.retyperAll class C {
      class A[T]
      implicit val c = { val s = ""; new A[s.type] }
      def test[T](implicit c: A[T]): A[T] = ???

      val v = test
      val w: A[_ <: String with Singleton] = v
    }""" should compile
  }

  it should "typecheck partially applied functions" in {
    implicit val s = ""
    def test(x: Float)(c: Char)(implicit s: String): Int = { markUsed(s); ??? }

    markUsed(test(0)('a'))

    "TyperTester.retyper { val x = test(0) _; val y = x('a') }" should compile
    "TyperTester.retyperAll { val x = test(0) _; val y = x('a') }" should compile
    "@TyperTester.retyper class C { val x = test(0) _; val y = x('a') }" should compile
    "@TyperTester.retyperAll class C { val x = test(0) _; val y = x('a') }" should compile

    "TyperTester.retyper { val x: Char => Int = test(0) _; val y: Int = x('a') }" should compile
    "TyperTester.retyperAll { val x: Char => Int = test(0) _; val y: Int = x('a') }" should compile
    "@TyperTester.retyper class C { val x: Char => Int = test(0) _; val y: Int = x('a') }" should compile
    "@TyperTester.retyperAll class C { val x: Char => Int = test(0) _; val y: Int = x('a') }" should compile

    "TyperTester.retyper { val x: Char = test(0) _ }" shouldNot typeCheck
    "TyperTester.retyperAll { val x: Char = test(0) _ }" shouldNot typeCheck
    "@TyperTester.retyper class C { val x: Char = test(0) _ }" shouldNot typeCheck
    "@TyperTester.retyperAll class C { val x: Char = test(0) _ }" shouldNot typeCheck
  }

  it should "typecheck implicit in package of same name as current package" in {
    def test0(implicit d: retypecheck.DummyClass) = { markUsed(d); ??? }
    def test1(implicit d: retypecheck.retypecheck.DummyClass) = { markUsed(d); ??? }

    markUsed(test0)
    markUsed(test1)

    "TyperTester.retyper { test0 }" should compile
    "TyperTester.retyperAll { test0 }" should compile
    "@TyperTester.retyper class C { test0 }" should compile
    "@TyperTester.retyperAll class C { test0 }" should compile

    "TyperTester.retyper { test1 }" should compile
    "TyperTester.retyperAll { test1 }" should compile
    "@TyperTester.retyper class C { test1 }" should compile
    "@TyperTester.retyperAll class C { test1 }" should compile
  }

  it should "typecheck global object access with local method of same name" in {
    "TyperTester.retyper { def globalObject = 0; globalValue }" should compile
    "TyperTester.retyperAll { def globalObject = 0; globalValue }" should compile
    "@TyperTester.retyper class C { def globalObject = 0; globalValue }" should compile
    "@TyperTester.retyperAll class C { def globalObject = 0; globalValue }" should compile
  }

  it should "typecheck stable path with abstract value" in {
    trait WithTypeMember { type U }

    markUsed(new WithTypeMember { })

    "TyperTester.retyper { trait T { val v: WithTypeMember; type V <: v.U } }" should compile
    "TyperTester.retyperAll { trait T { val v: WithTypeMember; type V <: v.U } }" should compile
    "@TyperTester.retyper trait T { val v: WithTypeMember; type V <: v.U }" should compile
    "@TyperTester.retyperAll trait T { val v: WithTypeMember; type V <: v.U }" should compile
  }

  it should "correctly compile case classes and objects in function literals" in {
    val v1 = TyperTester.retyper {
      val v = { argument: Int =>
        case class TraitCase(a: Trait)

        trait Trait
        case class IntCase(i: Int) extends Trait
        case class StringCase(s: String, t: TraitCase) extends Trait
        case object CaseObject extends Trait

        val instance: Trait = StringCase("", TraitCase(IntCase(2)))
        val i = instance match {
          case IntCase(i) => i + argument + 1
          case StringCase(_, TraitCase(IntCase(i))) => i + argument + 1
          case CaseObject => argument + 2
        }

        case class C(s: String)(j: Int) extends A(j) { def a = 9 }
        case object D { def x = "value" }

        D.x + (2 * i + C("")(0).a)
      }
      v
    }

    val v2 = TyperTester.retyperAll {
      val v = { argument: Int =>
        case class TraitCase(a: Trait)

        trait Trait
        case class IntCase(i: Int) extends Trait
        case class StringCase(s: String, t: TraitCase) extends Trait
        case object CaseObject extends Trait

        val instance: Trait = StringCase("", TraitCase(IntCase(2)))
        val i = instance match {
          case IntCase(i) => i + argument + 1
          case StringCase(_, TraitCase(IntCase(i))) => i + argument + 1
          case CaseObject => argument + 2
        }

        case class C(s: String)(j: Int) extends A(j) { def a = 9 }
        case object D { def x = "value" }

        D.x + (2 * i + C("")(0).a)
      }
      v
    }

    @TyperTester.retyper object o1 {
      val v = { argument: Int =>
        case class TraitCase(a: Trait)

        trait Trait
        case class IntCase(i: Int) extends Trait
        case class StringCase(s: String, t: TraitCase) extends Trait
        case object CaseObject extends Trait

        val instance: Trait = StringCase("", TraitCase(IntCase(2)))
        val i = instance match {
          case IntCase(i) => i + argument + 1
          case StringCase(_, TraitCase(IntCase(i))) => i + argument + 1
          case CaseObject => argument + 2
        }

        case class C(s: String)(j: Int) extends A(j) { def a = 9 }
        case object D { def x = "value" }

        D.x + (2 * i + C("")(0).a)
      }
    }

    @TyperTester.retyperAll object o2 {
      val v = { argument: Int =>
        case class TraitCase(a: Trait)

        trait Trait
        case class IntCase(i: Int) extends Trait
        case class StringCase(s: String, t: TraitCase) extends Trait
        case object CaseObject extends Trait

        val instance: Trait = StringCase("", TraitCase(IntCase(2)))
        val i = instance match {
          case IntCase(i) => i + argument + 1
          case StringCase(_, TraitCase(IntCase(i))) => i + argument + 1
          case CaseObject => argument + 2
        }

        case class C(s: String)(j: Int) extends A(j) { def a = 9 }
        case object D { def x = "value" }

        D.x + (2 * i + C("")(0).a)
      }
    }

    v1(42) should be ("value99")
    v2(42) should be ("value99")
    o1.v(42) should be ("value99")
    o2.v(42) should be ("value99")
  }

  it should "correctly compile members of abstract type member" in {
    trait T1 {
      type A
      def d: A
      val v = TyperTester.retyper { d }
    }
    object o1 extends T1 {
      type A = Int
      def d = 8
    }

    trait T2 {
      type A
      def d: A
      val v = TyperTester.retyperAll { d }
    }
    object o2 extends T2 {
      type A = Int
      def d = 8
    }

    @TyperTester.retyperAll trait T3 {
      type A
      def d: A
      val v = d
    }
    object o3 extends T3 {
      type A = Int
      def d = 8
    }

    @TyperTester.retyperAll trait T4 {
      type A
      def d: A
      val v = d
    }
    object o4 extends T4 {
      type A = Int
      def d = 8
    }

    o1.v should be (8)
    o2.v should be (8)
    o3.v should be (8)
    o4.v should be (8)
  }

  it should "correctly compile default arguments" in {
    val v1 = TyperTester.retyper {
      class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
        def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
          i + j + k + l + m + n + o + p
      }
      new C(6, k = 5).a(60, o = 50)
    }

    val v2 = TyperTester.retyperAll {
      class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
        def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
          i + j + k + l + m + n + o + p
      }
      new C(6, k = 5).a(60, o = 50)
    }

    @TyperTester.retyper object o1 {
      class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
        def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
          i + j + k + l + m + n + o + p
      }
      val v = new C(6, k = 5).a(60, o = 50)
    }

    val o1v = new o1.C(6, k = 5).a(60, o = 50)

    @TyperTester.retyperAll object o2 {
      class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
        def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
          i + j + k + l + m + n + o + p
      }
      val v = new C(6, k = 5).a(60, o = 50)
    }

    val o2v = new o2.C(6, k = 5).a(60, o = 50)

    v1 should be (297)
    v2 should be (297)
    o1v should be (297)
    o2v should be (297)
    o1.v should be (297)
    o2.v should be (297)
  }

  it should "correctly compile auxiliaries and default arguments" in {
    val v1 = TyperTester.retyper {
      class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
        def this(i: String, k: Int) = this(i.length, k = k)
        def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
          i + j + k + l + m + n + o + p
      }
      (new C(6, k = 5).a(60, o = 50), new C("123456", 5).a(60, o = 50))
    }

    val v2 = TyperTester.retyperAll {
      class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
        def this(i: String, k: Int) = this(i.length, k = k)
        def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
          i + j + k + l + m + n + o + p
      }
      (new C(6, k = 5).a(60, o = 50), new C("123456", 5).a(60, o = 50))
    }

    @TyperTester.retyper object o1 {
      class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
        def this(i: String, k: Int) = this(i.length, k = k)
        def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
          i + j + k + l + m + n + o + p
      }
      val v = (new C(6, k = 5).a(60, o = 50), new C("123456", 5).a(60, o = 50))
    }

    val o1v = (new o1.C(6, k = 5).a(60, o = 50), new o1.C("123456", 5).a(60, o = 50))

    @TyperTester.retyperAll object o2 {
      class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
        def this(i: String, k: Int) = this(i.length, k = k)
        def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
          i + j + k + l + m + n + o + p
      }
      val v = (new C(6, k = 5).a(60, o = 50), new C("123456", 5).a(60, o = 50))
    }

    val o2v = (new o2.C(6, k = 5).a(60, o = 50), new o2.C("123456", 5).a(60, o = 50))

    v1 should be ((297, 297))
    v2 should be ((297, 297))
    o1v should be ((297, 297))
    o2v should be ((297, 297))
    o1.v should be ((297, 297))
    o2.v should be ((297, 297))
  }

  it should "correctly compile nesting with default arguments" in {
    val v1 = TyperTester.retyper {
      class C {
        class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
          def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
            i + j + k + l + m + n + o + p
        }
        val v = new C(6, k = 5).a(60, o = 50)
      }
      (new C).v
    }

    val v2 = TyperTester.retyperAll {
      class C {
        class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
          def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
            i + j + k + l + m + n + o + p
        }
        val v = new C(6, k = 5).a(60, o = 50)
      }
      (new C).v
    }

    @TyperTester.retyper object o1 {
      class C {
        class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
          def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
            i + j + k + l + m + n + o + p
        }
        val v = new C(6, k = 5).a(60, o = 50)
      }
      val v = (new C).v
    }

    val o1v = {
      val c = new o1.C
      new c.C(6, k = 5).a(60, o = 50)
    }

    @TyperTester.retyperAll object o2 {
      class C {
        class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
          def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
            i + j + k + l + m + n + o + p
        }
        val v = new C(6, k = 5).a(60, o = 50)
      }
      val v = (new C).v
    }

    val o2v = {
      val c = new o2.C
      new c.C(6, k = 5).a(60, o = 50)
    }

    v1 should be (297)
    v2 should be (297)
    o1v should be (297)
    o2v should be (297)
    o1.v should be (297)
    o2.v should be (297)
  }

  it should "correctly compile nesting with auxiliaries and default arguments" in {
    val v1 = TyperTester.retyper {
      class C {
        class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
          def this(i: String, k: Int) = this(i.length, k = k)
          def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
            i + j + k + l + m + n + o + p
        }
        val v = (new C(6, k = 5).a(60, o = 50), new C("123456", 5).a(60, o = 50))
      }
      (new C).v
    }

    val v2 = TyperTester.retyperAll {
      class C {
        class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
          def this(i: String, k: Int) = this(i.length, k = k)
          def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
            i + j + k + l + m + n + o + p
        }
        val v = (new C(6, k = 5).a(60, o = 50), new C("123456", 5).a(60, o = 50))
      }
      (new C).v
    }

    @TyperTester.retyper object o1 {
      class C {
        class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
          def this(i: String, k: Int) = this(i.length, k = k)
          def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
            i + j + k + l + m + n + o + p
        }
        val v = (new C(6, k = 5).a(60, o = 50), new C("123456", 5).a(60, o = 50))
      }
      val v = (new C).v
    }

    val o1v = {
      val c = new o1.C
      (new c.C(6, k = 5).a(60, o = 50), new c.C("123456", 5).a(60, o = 50))
    }

    @TyperTester.retyperAll object o2 {
      class C {
        class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
          def this(i: String, k: Int) = this(i.length, k = k)
          def a(m: Int, n: Int = 70, o: Int = 80, p: Int = 90) =
            i + j + k + l + m + n + o + p
        }
        val v = (new C(6, k = 5).a(60, o = 50), new C("123456", 5).a(60, o = 50))
      }
      val v = (new C).v
    }

    val o2v = {
      val c = new o2.C
      (new c.C(6, k = 5).a(60, o = 50), new c.C("123456", 5).a(60, o = 50))
    }

    v1 should be ((297, 297))
    v2 should be ((297, 297))
    o1v should be ((297, 297))
    o2v should be ((297, 297))
    o1.v should be ((297, 297))
    o2.v should be ((297, 297))
  }

  it should "correctly compile private default arguments" in {
    val v1 = TyperTester.retyper {
      class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
        private def double(x: Int) = 2 * x
        def a(m: Int, n: Int = double(35), o: Int = double(40), p: Int = double(45)) =
          i + j + k + l + m + n + o + p
      }
      new C(6, k = 5).a(60, o = 50)
    }

    val v2 = TyperTester.retyperAll {
      class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
        private def double(x: Int) = 2 * x
        def a(m: Int, n: Int = double(35), o: Int = double(40), p: Int = double(45)) =
          i + j + k + l + m + n + o + p
      }
      new C(6, k = 5).a(60, o = 50)
    }

    @TyperTester.retyper object o1 {
      class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
        private def double(x: Int) = 2 * x
        def a(m: Int, n: Int = double(35), o: Int = double(40), p: Int = double(45)) =
          i + j + k + l + m + n + o + p
      }
      val v = new C(6, k = 5).a(60, o = 50)
    }

    val o1v = new o1.C(6, k = 5).a(60, o = 50)

    @TyperTester.retyperAll object o2 {
      class C(i: Int, j: Int = 7, k: Int = 8, l: Int = 9) {
        private def double(x: Int) = 2 * x
        def a(m: Int, n: Int = double(35), o: Int = double(40), p: Int = double(45)) =
          i + j + k + l + m + n + o + p
      }
      val v = new C(6, k = 5).a(60, o = 50)
    }

    val o2v = new o2.C(6, k = 5).a(60, o = 50)

    v1 should be (297)
    v2 should be (297)
    o1v should be (297)
    o2v should be (297)
    o1.v should be (297)
    o2.v should be (297)
  }

  it should "correctly compile anonymous functions and implicits" in {
    val v1 = TyperTester.retyper {
      trait Defaults {
        trait Default[T] { def f(v: T): T }
        implicit object default extends Default[Int] { def f(v: Int) = v + 5 }
        def test[T: Default](g: () => T) = implicitly[Default[T]] f g()
      }

      trait Trait extends Defaults {
        val v = test(() => 7)
      }

      new Trait { }.v
    }

    val v2 = TyperTester.retyperAll {
      trait Defaults {
        trait Default[T] { def f(v: T): T }
        implicit object default extends Default[Int] { def f(v: Int) = v + 5 }
        def test[T: Default](g: () => T) = implicitly[Default[T]] f g()
      }

      trait Trait extends Defaults {
        val v = test(() => 7)
      }

      new Trait { }.v
    }

    @TyperTester.retyper object o1 {
      trait Defaults {
        trait Default[T] { def f(v: T): T }
        implicit object default extends Default[Int] { def f(v: Int) = v + 5 }
        def test[T: Default](g: () => T) = implicitly[Default[T]] f g()
      }

      trait Trait extends Defaults {
        val v = test(() => 7)
      }

      val v = new Trait { }.v
    }

    @TyperTester.retyperAll object o2 {
      trait Defaults {
        trait Default[T] { def f(v: T): T }
        implicit object default extends Default[Int] { def f(v: Int) = v + 5 }
        def test[T: Default](g: () => T) = implicitly[Default[T]] f g()
      }

      trait Trait extends Defaults {
        val v = test(() => 7)
      }

      val v = new Trait { }.v
    }

    v1 should be (12)
    v2 should be (12)
    o1.v should be (12)
    o2.v should be (12)
  }
}
