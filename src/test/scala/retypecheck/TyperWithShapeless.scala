package retypecheck

import org.scalatest._
import shapeless._

class TyperWithShapelessSpec extends FlatSpec with Matchers {
  case class Foo(i: Int, s: String, b: Boolean)
  val foo = Foo(23, "foo", true)

  sealed trait List[+T]
  case class Cons[T](hd: T, tl: List[T]) extends List[T]
  sealed trait Nil extends List[Nothing]
  case object Nil extends Nil

  def markUsed(v: => Any) = ()

  behavior of "Typer for code using Shapeless examples"

  import poly._

  it should "typecheck heterogenous lists" in {
    """TyperTester.retyper {
      object choose extends (Set ~> Option) {
        def apply[T](s : Set[T]) = s.headOption
      }

      val a0 = Set(1) :: Set("foo") :: HNil
      val a1 = a0(0)

      val b0 = a0 map choose
      val b1 = b0(1)

      val x0: Set[Int] :: Set[String] :: HNil = a0
      val x1: Set[Int] = a1
      val y0: Option[Int] :: Option[String] :: HNil = b0
      val y1: Option[String] = b1
    }""" should compile

    """TyperTester.retyperAll {
      object choose extends (Set ~> Option) {
        def apply[T](s : Set[T]) = s.headOption
      }

      val a0 = Set(1) :: Set("foo") :: HNil
      val a1 = a0(0)

      val b0 = a0 map choose
      val b1 = b0(1)

      val x0: Set[Int] :: Set[String] :: HNil = a0
      val x1: Set[Int] = a1
      val y0: Option[Int] :: Option[String] :: HNil = b0
      val y1: Option[String] = b1
    }""" should compile

    """@TyperTester.retyper class C {
      object choose extends (Set ~> Option) {
        def apply[T](s : Set[T]) = s.headOption
      }

      val a0 = Set(1) :: Set("foo") :: HNil
      val a1 = a0(0)

      val b0 = a0 map choose
      val b1 = b0(1)

      val x0: Set[Int] :: Set[String] :: HNil = a0
      val x1: Set[Int] = a1
      val y0: Option[Int] :: Option[String] :: HNil = b0
      val y1: Option[String] = b1
    }""" should compile

    """@TyperTester.retyperAll class C {
      object choose extends (Set ~> Option) {
        def apply[T](s : Set[T]) = s.headOption
      }

      val a0 = Set(1) :: Set("foo") :: HNil
      val a1 = a0(0)

      val b0 = a0 map choose
      val b1 = b0(1)

      val x0: Set[Int] :: Set[String] :: HNil = a0
      val x1: Set[Int] = a1
      val y0: Option[Int] :: Option[String] :: HNil = b0
      val y1: Option[String] = b1
    }""" should compile
  }

  import syntax.zipper._

  it should "typecheck zippers for heterogenous lists" in {
    """TyperTester.retyper {
      val l = 1 :: "foo" :: 3.0 :: HNil

      val a0 = l.toZipper.right.put(("wibble", 45)).reify
      val a1: Int :: (String, Int) :: Double :: HNil = a0

      val b0 = l.toZipper.right.delete.reify
      val b1: Int :: Double :: HNil = b0

      val c0 = l.toZipper.last.left.insert("bar").reify
      val c1: Int :: String :: String :: Double :: HNil = c0
    }""" should compile

    """TyperTester.retyperAll {
      val l = 1 :: "foo" :: 3.0 :: HNil

      val a0 = l.toZipper.right.put(("wibble", 45)).reify
      val a1: Int :: (String, Int) :: Double :: HNil = a0

      val b0 = l.toZipper.right.delete.reify
      val b1: Int :: Double :: HNil = b0

      val c0 = l.toZipper.last.left.insert("bar").reify
      val c1: Int :: String :: String :: Double :: HNil = c0
    }""" should compile

    """@TyperTester.retyper class C {
      val l = 1 :: "foo" :: 3.0 :: HNil

      val a0 = l.toZipper.right.put(("wibble", 45)).reify
      val a1: Int :: (String, Int) :: Double :: HNil = a0

      val b0 = l.toZipper.right.delete.reify
      val b1: Int :: Double :: HNil = b0

      val c0 = l.toZipper.last.left.insert("bar").reify
      val c1: Int :: String :: String :: Double :: HNil = c0
    }""" should compile

    """@TyperTester.retyperAll class C {
      val l = 1 :: "foo" :: 3.0 :: HNil

      val a0 = l.toZipper.right.put(("wibble", 45)).reify
      val a1: Int :: (String, Int) :: Double :: HNil = a0

      val b0 = l.toZipper.right.delete.reify
      val b1: Int :: Double :: HNil = b0

      val c0 = l.toZipper.last.left.insert("bar").reify
      val c1: Int :: String :: String :: Double :: HNil = c0
    }""" should compile
  }

  it should "typecheck statically sized collections" in {
    def row(cols: Seq[String]) =
      cols.mkString("\"", "\", \"", "\"")
    def csv[N <: Nat](hdrs: Sized[Seq[String], N], rows: Seq[Sized[Seq[String], N]]) =
      row(hdrs) :+ rows.map(row(_))

    markUsed(csv(Sized(""), Seq(Sized(""))))

    """TyperTester.retyper {
      val hdrs = Sized("Title", "Author")
      val rows = Seq(
        Sized("Types and Programming Languages", "Benjamin Pierce"),
        Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones"))
      csv(hdrs, rows)
    }""" should compile

    """TyperTester.retyperAll {
      val hdrs = Sized("Title", "Author")
      val rows = Seq(
        Sized("Types and Programming Languages", "Benjamin Pierce"),
        Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones"))
      csv(hdrs, rows)
    }""" should compile

    """@TyperTester.retyper class C {
      val hdrs = Sized("Title", "Author")
      val rows = Seq(
        Sized("Types and Programming Languages", "Benjamin Pierce"),
        Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones"))
      csv(hdrs, rows)
    }""" should compile

    """@TyperTester.retyperAll class C {
      val hdrs = Sized("Title", "Author")
      val rows = Seq(
        Sized("Types and Programming Languages", "Benjamin Pierce"),
        Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones"))
      csv(hdrs, rows)
    }""" should compile

    """TyperTester.retyper {
      val hdrs = Sized("Title", "Author", "ISBN")
      val rows = Seq(
        Sized("Types and Programming Languages", "Benjamin Pierce"),
        Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones"))
      csv(hdrs, rows)
    }""" shouldNot typeCheck

    """TyperTester.retyperAll {
      val hdrs = Sized("Title", "Author", "ISBN")
      val rows = Seq(
        Sized("Types and Programming Languages", "Benjamin Pierce"),
        Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones"))
      csv(hdrs, rows)
    }""" shouldNot typeCheck

    """@TyperTester.retyper class C {
      val hdrs = Sized("Title", "Author", "ISBN")
      val rows = Seq(
        Sized("Types and Programming Languages", "Benjamin Pierce"),
        Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones"))
      csv(hdrs, rows)
    }""" shouldNot typeCheck

    """@TyperTester.retyperAll class C {
      val hdrs = Sized("Title", "Author", "ISBN")
      val rows = Seq(
        Sized("Types and Programming Languages", "Benjamin Pierce"),
        Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones"))
      csv(hdrs, rows)
    }""" shouldNot typeCheck
  }

  import syntax.singleton._
  import record._

  it should "correctly compile extensible records" in {
    val v1 = TyperTester.retyper {
      val book =
        ("author" ->> "Benjamin Pierce") ::
        ("title"  ->> "Types and Programming Languages") ::
        ("id"     ->>  262162091) ::
        ("price"  ->>  44.11) ::
        HNil
      (book("author"), book("title"), book("id"), book("price"))
    }

    val v2 = TyperTester.retyperAll {
      val book =
        ("author" ->> "Benjamin Pierce") ::
        ("title"  ->> "Types and Programming Languages") ::
        ("id"     ->>  262162091) ::
        ("price"  ->>  44.11) ::
        HNil
      (book("author"), book("title"), book("id"), book("price"))
    }

    @TyperTester.retyper object o1 {
      val book =
        ("author" ->> "Benjamin Pierce") ::
        ("title"  ->> "Types and Programming Languages") ::
        ("id"     ->>  262162091) ::
        ("price"  ->>  44.11) ::
        HNil
      val v = (book("author"), book("title"), book("id"), book("price"))
    }

    @TyperTester.retyperAll object o2 {
      val book =
        ("author" ->> "Benjamin Pierce") ::
        ("title"  ->> "Types and Programming Languages") ::
        ("id"     ->>  262162091) ::
        ("price"  ->>  44.11) ::
        HNil
      val v = (book("author"), book("title"), book("id"), book("price"))
    }

    v1 should be (("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11))
    v2 should be (("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11))
    o1.v should be (("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11))
    o2.v should be (("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11))
  }

  it should "correctly compile generic representation of outer case classes" in {
    val v1 = TyperTester.retyper {
      val fooGen = Generic[Foo]
      val res0 = fooGen.to(foo)
      val res1 = fooGen.from(13 :: res0.tail)
      (res0, res1)
    }

    val v2 = TyperTester.retyperAll {
      val fooGen = Generic[Foo]
      val res0 = fooGen.to(foo)
      val res1 = fooGen.from(13 :: res0.tail)
      (res0, res1)
    }

    @TyperTester.retyper class C1 {
      val fooGen = Generic[Foo]
      val res0 = fooGen.to(foo)
      val res1 = fooGen.from(13 :: res0.tail)
      val v = (res0, res1)
    }

    @TyperTester.retyperAll class C2 {
      val fooGen = Generic[Foo]
      val res0 = fooGen.to(foo)
      val res1 = fooGen.from(13 :: res0.tail)
      val v = (res0, res1)
    }

    v1 should be ((23 :: "foo" :: true :: shapeless.HNil, Foo(13, "foo", true)))
    v2 should be ((23 :: "foo" :: true :: shapeless.HNil, Foo(13, "foo", true)))
    (new C1).v should be ((23 :: "foo" :: true :: shapeless.HNil, Foo(13, "foo", true)))
    (new C2).v should be ((23 :: "foo" :: true :: shapeless.HNil, Foo(13, "foo", true)))
  }

  it should "correctly compile generic representation of inner case classes" in {
    val v1 = TyperTester.retyper {
      case class Bar(i: Int, s: String, b: Boolean)
      val bar = Bar(23, "foo", true)
      val barGen = Generic[Bar]
      barGen.to(barGen.from(13 :: barGen.to(bar).tail).copy(s = "bar"))
    }

    val v2 = TyperTester.retyperAll {
      case class Bar(i: Int, s: String, b: Boolean)
      val bar = Bar(23, "foo", true)
      val barGen = Generic[Bar]
      barGen.to(barGen.from(13 :: barGen.to(bar).tail).copy(s = "bar"))
    }

    @TyperTester.retyper class C1 {
      case class Bar(i: Int, s: String, b: Boolean)
      val bar = Bar(23, "foo", true)
      val barGen = Generic[Bar]
      val v = barGen.to(barGen.from(13 :: barGen.to(bar).tail).copy(s = "bar"))
    }

    @TyperTester.retyperAll class C2 {
      case class Bar(i: Int, s: String, b: Boolean)
      val bar = Bar(23, "foo", true)
      val barGen = Generic[Bar]
      val v = barGen.to(barGen.from(13 :: barGen.to(bar).tail).copy(s = "bar"))
    }

    v1 should be (13 :: "bar" :: true :: shapeless.HNil)
    v2 should be (13 :: "bar" :: true :: shapeless.HNil)
    (new C1).v should be (13 :: "bar" :: true :: shapeless.HNil)
    (new C2).v should be (13 :: "bar" :: true :: shapeless.HNil)
  }

  it should "correctly compile lazy implicit arguments" in {
    val v1 = TyperTester.retyper {
      trait Show[T] { def apply(t: T): String }
      object Show {
        implicit def showInt: Show[Int] = new Show[Int] { def apply(t: Int) = t.toString }
        implicit def showNil: Show[Nil] = new Show[Nil] { def apply(t: Nil) = "Nil" }
        implicit def showCons[T](implicit st: Lazy[Show[T]], sl: Lazy[Show[List[T]]]): Show[Cons[T]] = new Show[Cons[T]] {
          def apply(t: Cons[T]) = s"Cons(${show(t.hd)(st.value)}, ${show(t.tl)(sl.value)})"
        }
        implicit def showList[T](implicit sc: Lazy[Show[Cons[T]]]): Show[List[T]] = new Show[List[T]] {
          def apply(t: List[T]) = t match {
            case n: Nil => show(n)
            case c: Cons[T] => show(c)(sc.value)
          }
        }
      }

      def show[T](t: T)(implicit s: Show[T]) = s(t)
      show(Cons(1, Cons(2, Cons(3, Nil))))
    }

    val v2 = TyperTester.retyperAll {
      trait Show[T] { def apply(t: T): String }
      object Show {
        implicit def showInt: Show[Int] = new Show[Int] { def apply(t: Int) = t.toString }
        implicit def showNil: Show[Nil] = new Show[Nil] { def apply(t: Nil) = "Nil" }
        implicit def showCons[T](implicit st: Lazy[Show[T]], sl: Lazy[Show[List[T]]]): Show[Cons[T]] = new Show[Cons[T]] {
          def apply(t: Cons[T]) = s"Cons(${show(t.hd)(st.value)}, ${show(t.tl)(sl.value)})"
        }
        implicit def showList[T](implicit sc: Lazy[Show[Cons[T]]]): Show[List[T]] = new Show[List[T]] {
          def apply(t: List[T]) = t match {
            case n: Nil => show(n)
            case c: Cons[T] => show(c)(sc.value)
          }
        }
      }

      def show[T](t: T)(implicit s: Show[T]) = s(t)
      show(Cons(1, Cons(2, Cons(3, Nil))))
    }

    @TyperTester.retyper object o1 {
      trait Show[T] { def apply(t: T): String }
      object Show {
        implicit def showInt: Show[Int] = new Show[Int] { def apply(t: Int) = t.toString }
        implicit def showNil: Show[Nil] = new Show[Nil] { def apply(t: Nil) = "Nil" }
        implicit def showCons[T](implicit st: Lazy[Show[T]], sl: Lazy[Show[List[T]]]): Show[Cons[T]] = new Show[Cons[T]] {
          def apply(t: Cons[T]) = s"Cons(${show(t.hd)(st.value)}, ${show(t.tl)(sl.value)})"
        }
        implicit def showList[T](implicit sc: Lazy[Show[Cons[T]]]): Show[List[T]] = new Show[List[T]] {
          def apply(t: List[T]) = t match {
            case n: Nil => show(n)
            case c: Cons[T] => show(c)(sc.value)
          }
        }
      }

      def show[T](t: T)(implicit s: Show[T]) = s(t)
      val v = show(Cons(1, Cons(2, Cons(3, Nil))))
    }

    @TyperTester.retyperAll object o2 {
      trait Show[T] { def apply(t: T): String }
      object Show {
        implicit def showInt: Show[Int] = new Show[Int] { def apply(t: Int) = t.toString }
        implicit def showNil: Show[Nil] = new Show[Nil] { def apply(t: Nil) = "Nil" }
        implicit def showCons[T](implicit st: Lazy[Show[T]], sl: Lazy[Show[List[T]]]): Show[Cons[T]] = new Show[Cons[T]] {
          def apply(t: Cons[T]) = s"Cons(${show(t.hd)(st.value)}, ${show(t.tl)(sl.value)})"
        }
        implicit def showList[T](implicit sc: Lazy[Show[Cons[T]]]): Show[List[T]] = new Show[List[T]] {
          def apply(t: List[T]) = t match {
            case n: Nil => show(n)
            case c: Cons[T] => show(c)(sc.value)
          }
        }
      }

      def show[T](t: T)(implicit s: Show[T]) = s(t)
      val v = show(Cons(1, Cons(2, Cons(3, Nil))))
    }

    v1 should be ("Cons(1, Cons(2, Cons(3, Nil)))")
    v2 should be ("Cons(1, Cons(2, Cons(3, Nil)))")
    o1.v should be ("Cons(1, Cons(2, Cons(3, Nil)))")
    o2.v should be ("Cons(1, Cons(2, Cons(3, Nil)))")
  }
}
