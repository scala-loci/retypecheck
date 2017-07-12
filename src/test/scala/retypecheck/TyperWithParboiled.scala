package retypecheck

import org.scalatest._
import org.parboiled2._
import scala.util.Success

class TyperWithParboiled extends FlatSpec with Matchers {
  behavior of "Typer for code using parboiled2 examples"

  it should "correctly compile parser for simple integer expressions" in {
    val v1 = TyperTester.retyper {
      class Calculator(val input: ParserInput) extends Parser {
        def InputLine = rule { Expression ~ EOI }
        def Expression: Rule1[Int] = rule { Term ~ zeroOrMore('+' ~ Term ~> ((_: Int) + _) | '-' ~ Term ~> ((_: Int) - _)) }
        def Term = rule { Factor ~ zeroOrMore('*' ~ Factor ~> ((_: Int) * _) | '/' ~ Factor ~> ((_: Int) / _)) }
        def Factor = rule { Number | Parens }
        def Parens = rule { '(' ~ Expression ~ ')' }
        def Number = rule { capture(Digits) ~> (_.toInt) }
        def Digits = rule { oneOrMore(CharPredicate.Digit) }
      }
      new Calculator("1+1").InputLine.run()
    }

    val v2 = TyperTester.retyperAll {
      class Calculator(val input: ParserInput) extends Parser {
        def InputLine = rule { Expression ~ EOI }
        def Expression: Rule1[Int] = rule { Term ~ zeroOrMore('+' ~ Term ~> ((_: Int) + _) | '-' ~ Term ~> ((_: Int) - _)) }
        def Term = rule { Factor ~ zeroOrMore('*' ~ Factor ~> ((_: Int) * _) | '/' ~ Factor ~> ((_: Int) / _)) }
        def Factor = rule { Number | Parens }
        def Parens = rule { '(' ~ Expression ~ ')' }
        def Number = rule { capture(Digits) ~> (_.toInt) }
        def Digits = rule { oneOrMore(CharPredicate.Digit) }
      }
      new Calculator("1+1").InputLine.run()
    }

    @TyperTester.retyper object o1 {
      class Calculator(val input: ParserInput) extends Parser {
        def InputLine = rule { Expression ~ EOI }
        def Expression: Rule1[Int] = rule { Term ~ zeroOrMore('+' ~ Term ~> ((_: Int) + _) | '-' ~ Term ~> ((_: Int) - _)) }
        def Term = rule { Factor ~ zeroOrMore('*' ~ Factor ~> ((_: Int) * _) | '/' ~ Factor ~> ((_: Int) / _)) }
        def Factor = rule { Number | Parens }
        def Parens = rule { '(' ~ Expression ~ ')' }
        def Number = rule { capture(Digits) ~> (_.toInt) }
        def Digits = rule { oneOrMore(CharPredicate.Digit) }
      }
      val v = new Calculator("1+1").InputLine.run()
    }

    @TyperTester.retyperAll object o2 {
      class Calculator(val input: ParserInput) extends Parser {
        def InputLine = rule { Expression ~ EOI }
        def Expression: Rule1[Int] = rule { Term ~ zeroOrMore('+' ~ Term ~> ((_: Int) + _) | '-' ~ Term ~> ((_: Int) - _)) }
        def Term = rule { Factor ~ zeroOrMore('*' ~ Factor ~> ((_: Int) * _) | '/' ~ Factor ~> ((_: Int) / _)) }
        def Factor = rule { Number | Parens }
        def Parens = rule { '(' ~ Expression ~ ')' }
        def Number = rule { capture(Digits) ~> (_.toInt) }
        def Digits = rule { oneOrMore(CharPredicate.Digit) }
      }
      val v = new Calculator("1+1").InputLine.run()
    }

    v1 should be (Success(2))
    v2 should be (Success(2))
    o1.v should be (Success(2))
    o2.v should be (Success(2))
  }
}
