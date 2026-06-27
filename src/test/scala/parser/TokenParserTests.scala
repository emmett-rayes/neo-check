package neocheck
package parser

import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}

/** Interpreter-agnostic unit tests against the [[ParserPrograms]] catalogue.
 *
 * The suite is parameterized over the parser carrier and its [[ParserAlgebra]] interpreter
 *
 * @tparam Parser the parser carrier produced by the interpreter.
 */
abstract class TokenParserTests[Parser[_]](interpreter: ParserAlgebra[Parser]) extends AnyFunSuite {

  /** Converts a String to [[Tokens]] and drives the parser produced by the interpreter. */
  protected def run[A](program: Parser[A], input: String): ParserResult[Tokens, A]

  protected given ParserAlgebra[Parser] = interpreter

  // ── literalHello ──────────────────────────────────────────────────────────

  test("literalHello: succeeds and consumes 'hello' exactly") {
    run(ParserPrograms.literalHello, "hello") match {
      case Success((remaining, parsed)) =>
        assert(parsed == "hello"): Unit
        assert(remaining.isEmpty)
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("literalHello: skips leading whitespace before matching") {
    run(ParserPrograms.literalHello, "  hello") match {
      case Success((_, parsed)) => assert(parsed == "hello")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("literalHello: leaves trailing input unconsumed") {
    run(ParserPrograms.literalHello, "hello world") match {
      case Success((remaining, _)) => assert(remaining.mkString == " world")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("literalHello: fails when input does not start with 'hello'") {
    assert(run(ParserPrograms.literalHello, "world").isFailure)
  }

  test("literalHello: fails on empty input") {
    assert(run(ParserPrograms.literalHello, "").isFailure)
  }

  // ── regexDigits ───────────────────────────────────────────────────────────

  test("regexDigits: matches a run of digits") {
    run(ParserPrograms.regexDigits, "123") match {
      case Success((_, parsed)) => assert(parsed == "123")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("regexDigits: stops at the first non-digit character") {
    run(ParserPrograms.regexDigits, "42abc") match {
      case Success((remaining, parsed)) =>
        assert(parsed == "42"): Unit
        assert(remaining.mkString == "abc")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("regexDigits: fails when input starts with non-digit characters") {
    assert(run(ParserPrograms.regexDigits, "abc").isFailure)
  }

  test("regexDigits: fails on empty input") {
    assert(run(ParserPrograms.regexDigits, "").isFailure)
  }

  // ── constantSuccess ───────────────────────────────────────────────────────

  test("constantSuccess: always succeeds with 42 without consuming input") {
    run(ParserPrograms.constantSuccess, "anything") match {
      case Success((remaining, parsed)) =>
        assert(parsed == 42): Unit
        assert(remaining.mkString == "anything")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("constantSuccess: succeeds even on empty input") {
    run(ParserPrograms.constantSuccess, "") match {
      case Success((_, parsed)) => assert(parsed == 42)
      case Failure(e) => fail(e.getMessage)
    }
  }

  // ── alwaysFailure ─────────────────────────────────────────────────────────

  test("alwaysFailure: always fails regardless of input") {
    assert(run(ParserPrograms.alwaysFailure, "hello").isFailure)
  }

  test("alwaysFailure: fails on empty input") {
    assert(run(ParserPrograms.alwaysFailure, "").isFailure)
  }

  test("alwaysFailure: carries the expected error message") {
    run(ParserPrograms.alwaysFailure, "") match {
      case Failure(e) => assert(e.getMessage.contains("boom"))
      case Success(_) => fail("Expected failure")
    }
  }

  // ── helloThenWorld ────────────────────────────────────────────────────────

  test("helloThenWorld: matches 'helloworld' and returns both halves") {
    run(ParserPrograms.helloThenWorld, "helloworld") match {
      case Success((remaining, (h, w))) =>
        assert(h == "hello"): Unit
        assert(w == "world"): Unit
        assert(remaining.isEmpty)
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("helloThenWorld: handles whitespace between the two tokens") {
    run(ParserPrograms.helloThenWorld, "hello world") match {
      case Success((_, (h, w))) =>
        assert(h == "hello"): Unit
        assert(w == "world")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("helloThenWorld: fails when 'world' is absent") {
    assert(run(ParserPrograms.helloThenWorld, "hello").isFailure)
  }

  test("helloThenWorld: fails on empty input") {
    assert(run(ParserPrograms.helloThenWorld, "").isFailure)
  }

  // ── trueOrFalse ───────────────────────────────────────────────────────────

  test("trueOrFalse: matches 'true'") {
    run(ParserPrograms.trueOrFalse, "true") match {
      case Success((_, parsed)) => assert(parsed == "true")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("trueOrFalse: matches 'false'") {
    run(ParserPrograms.trueOrFalse, "false") match {
      case Success((_, parsed)) => assert(parsed == "false")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("trueOrFalse: fails on any other token") {
    assert(run(ParserPrograms.trueOrFalse, "maybe").isFailure)
  }

  test("trueOrFalse: fails on empty input") {
    assert(run(ParserPrograms.trueOrFalse, "").isFailure)
  }

  // ── repeatedAb ────────────────────────────────────────────────────────────

  test("repeatedAb: yields an empty list on input that never matches") {
    run(ParserPrograms.repeatedAb, "xyz") match {
      case Success((_, parsed)) => assert(parsed == List.empty)
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("repeatedAb: yields an empty list on empty input") {
    run(ParserPrograms.repeatedAb, "") match {
      case Success((_, parsed)) => assert(parsed == List.empty)
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("repeatedAb: matches a single 'ab'") {
    run(ParserPrograms.repeatedAb, "ab") match {
      case Success((_, parsed)) => assert(parsed == List("ab"))
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("repeatedAb: matches multiple consecutive 'ab' occurrences") {
    run(ParserPrograms.repeatedAb, "ababab") match {
      case Success((_, parsed)) => assert(parsed == List("ab", "ab", "ab"))
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("repeatedAb: stops when the pattern no longer matches, leaving the rest") {
    run(ParserPrograms.repeatedAb, "ababc") match {
      case Success((remaining, parsed)) =>
        assert(parsed == List("ab", "ab")): Unit
        assert(remaining.mkString == "c")
      case Failure(e) => fail(e.getMessage)
    }
  }

  // ── atLeastTwoAb ─────────────────────────────────────────────────────────

  test("atLeastTwoAb: succeeds with exactly two 'ab'") {
    run(ParserPrograms.atLeastTwoAb, "abab") match {
      case Success((_, parsed)) => assert(parsed == List("ab", "ab"))
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("atLeastTwoAb: succeeds with more than two 'ab'") {
    run(ParserPrograms.atLeastTwoAb, "ababab") match {
      case Success((_, parsed)) => assert(parsed == List("ab", "ab", "ab"))
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("atLeastTwoAb: fails with only one 'ab'") {
    assert(run(ParserPrograms.atLeastTwoAb, "ab").isFailure)
  }

  test("atLeastTwoAb: fails on empty input") {
    assert(run(ParserPrograms.atLeastTwoAb, "").isFailure)
  }

  // ── digitsInParens ────────────────────────────────────────────────────────

  test("digitsInParens: parses digits surrounded by parentheses") {
    run(ParserPrograms.digitsInParens, "(123)") match {
      case Success((_, parsed)) => assert(parsed == "123")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("digitsInParens: handles whitespace before '('") {
    run(ParserPrograms.digitsInParens, "  (456)") match {
      case Success((_, parsed)) => assert(parsed == "456")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("digitsInParens: fails when parentheses are absent") {
    assert(run(ParserPrograms.digitsInParens, "123").isFailure)
  }

  test("digitsInParens: fails when the closing ')' is missing") {
    assert(run(ParserPrograms.digitsInParens, "(123").isFailure)
  }

  // ── prefixedDigits ────────────────────────────────────────────────────────

  test("prefixedDigits: skips the '>' and returns the digits") {
    run(ParserPrograms.prefixedDigits, ">42") match {
      case Success((_, parsed)) => assert(parsed == "42")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("prefixedDigits: fails when the '>' prefix is absent") {
    assert(run(ParserPrograms.prefixedDigits, "42").isFailure)
  }

  test("prefixedDigits: fails when no digits follow '>'") {
    assert(run(ParserPrograms.prefixedDigits, ">").isFailure)
  }

  // ── terminatedDigits ─────────────────────────────────────────────────────

  test("terminatedDigits: returns digits and discards the ';' terminator") {
    run(ParserPrograms.terminatedDigits, "99;") match {
      case Success((_, parsed)) => assert(parsed == "99")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("terminatedDigits: leaves input after ';' unconsumed") {
    run(ParserPrograms.terminatedDigits, "7; rest") match {
      case Success((remaining, parsed)) =>
        assert(parsed == "7"): Unit
        assert(remaining.mkString == " rest")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("terminatedDigits: fails when the ';' terminator is absent") {
    assert(run(ParserPrograms.terminatedDigits, "99").isFailure)
  }

  // ── digitListInParens ─────────────────────────────────────────────────────

  test("digitListInParens: parses a single-element list") {
    run(ParserPrograms.digitListInParens, "(42)") match {
      case Success((_, parsed)) => assert(parsed == List("42"))
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("digitListInParens: parses a multi-element list") {
    run(ParserPrograms.digitListInParens, "(1,22,333)") match {
      case Success((_, parsed)) => assert(parsed == List("1", "22", "333"))
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("digitListInParens: consumes the whole input when nothing is left") {
    run(ParserPrograms.digitListInParens, "(10,20)") match {
      case Success((remaining, _)) => assert(remaining.isEmpty)
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("digitListInParens: fails on empty parentheses") {
    assert(run(ParserPrograms.digitListInParens, "()").isFailure)
  }

  test("digitListInParens: fails when parentheses are absent") {
    assert(run(ParserPrograms.digitListInParens, "1,2,3").isFailure)
  }

  // ── leftAssociatedAs ──────────────────────────────────────────

  test("leftAssociatedAs: groups a run of 'a' to the left, seeded with the empty marker") {
    run(ParserPrograms.leftAssociatedAs, "aaa") match {
      case Success((remaining, parsed)) =>
        assert(parsed == "(((a)a)a)"): Unit
        assert(remaining.isEmpty)
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("leftAssociatedAs: combines a single 'a' with the seed") {
    run(ParserPrograms.leftAssociatedAs, "a") match {
      case Success((_, parsed)) => assert(parsed == "(a)")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("leftAssociatedAs: groups exactly two occurrences") {
    run(ParserPrograms.leftAssociatedAs, "aa") match {
      case Success((_, parsed)) => assert(parsed == "((a)a)")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("leftAssociatedAs: stops at the first non-'a', leaving the rest") {
    run(ParserPrograms.leftAssociatedAs, "aab") match {
      case Success((remaining, parsed)) =>
        assert(parsed == "((a)a)"): Unit
        assert(remaining.mkString == "b")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("leftAssociatedAs: yields the bare seed when no 'a' matches") {
    run(ParserPrograms.leftAssociatedAs, "xyz") match {
      case Success((remaining, parsed)) =>
        assert(parsed == ""): Unit
        assert(remaining.mkString == "xyz")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("leftAssociatedAs: yields the bare seed on empty input") {
    run(ParserPrograms.leftAssociatedAs, "") match {
      case Success((_, parsed)) => assert(parsed == "")
      case Failure(e) => fail(e.getMessage)
    }
  }

  // ── rightAssociatedAs ────────────────────────────────────────

  test("rightAssociatedAs: groups a run of 'a' to the right, seeded with the empty marker") {
    run(ParserPrograms.rightAssociatedAs, "aaa") match {
      case Success((remaining, parsed)) =>
        assert(parsed == "(a(a(a)))"): Unit
        assert(remaining.isEmpty)
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("rightAssociatedAs: combines a single 'a' with the seed") {
    run(ParserPrograms.rightAssociatedAs, "a") match {
      case Success((_, parsed)) => assert(parsed == "(a)")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("rightAssociatedAs: groups exactly two occurrences") {
    run(ParserPrograms.rightAssociatedAs, "aa") match {
      case Success((_, parsed)) => assert(parsed == "(a(a))")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("rightAssociatedAs: stops at the first non-'a', leaving the rest") {
    run(ParserPrograms.rightAssociatedAs, "aab") match {
      case Success((remaining, parsed)) =>
        assert(parsed == "(a(a))"): Unit
        assert(remaining.mkString == "b")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("rightAssociatedAs: yields the bare seed when no 'a' matches") {
    run(ParserPrograms.rightAssociatedAs, "xyz") match {
      case Success((remaining, parsed)) =>
        assert(parsed == ""): Unit
        assert(remaining.mkString == "xyz")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("rightAssociatedAs: yields the bare seed on empty input") {
    run(ParserPrograms.rightAssociatedAs, "") match {
      case Success((_, parsed)) => assert(parsed == "")
      case Failure(e) => fail(e.getMessage)
    }
  }

  // ── chainLeft vs chainRight ───────────────────────────────────────────────

  test("chainLeft and chainRight disagree on association for the same input") {
    val left = run(ParserPrograms.leftAssociatedAs, "aaaa")
    val right = run(ParserPrograms.rightAssociatedAs, "aaaa")
    (left, right) match {
      case (Success((_, l)), Success((_, r))) =>
        assert(l == "((((a)a)a)a)"): Unit
        assert(r == "(a(a(a(a))))"): Unit
        assert(l != r)
      case _ => fail("Expected both parsers to succeed")
    }
  }

  // ── additiveExpression ─────────
  test("additiveExpression: parses a lone term") {
    run(ParserPrograms.additiveExpression, "1") match {
      case Success((remaining, parsed)) =>
        assert(parsed == "n"): Unit
        assert(remaining.isEmpty)
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("additiveExpression: parses two terms separated by '+'") {
    run(ParserPrograms.additiveExpression, "1+2") match {
      case Success((_, parsed)) => assert(parsed == "(n+n)")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("additiveExpression: folds a chain of terms left-associatively") {
    run(ParserPrograms.additiveExpression, "11+22+333") match {
      case Success((remaining, parsed)) =>
        assert(parsed == "((n+n)+n)"): Unit
        assert(remaining.isEmpty)
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("additiveExpression: tolerates whitespace around terms and '+'") {
    run(ParserPrograms.additiveExpression, "  1 + 2 + 3 ") match {
      case Success((_, parsed)) => assert(parsed == "((n+n)+n)")
      case Failure(e) => fail(e.getMessage)
    }
  }

  test("additiveExpression: fails when no leading term is present") {
    assert(run(ParserPrograms.additiveExpression, "+1").isFailure)
  }

  test("additiveExpression: fails on empty input") {
    assert(run(ParserPrograms.additiveExpression, "").isFailure)
  }

  // ── rightRecursive ─────────────────────────────────────────
  test("rightRecursive: parses a right-recursive grammar") {
    run(ParserPrograms.rightRecursive, "1+2+3") match {
      case Success((remaining, parsed)) =>
        assert(parsed == "(n+(n+n))"): Unit
        assert(remaining.isEmpty)
      case Failure(e) => fail(e.getMessage)
    }
  }
}

/** Runs the shared [[TokenParserTests]] against the naive interpreter [[NaiveTokenParserInterpreter]]. */
class NaiveTokenParserTests extends TokenParserTests[TokenParser](NaiveTokenParserInterpreter()) {

  override protected def run[A](program: TokenParser[A], input: String): ParserResult[Tokens, A] =
    program.parse(input.asTokens)

  // ── leftRecursive ──────────────────────────────────────────────────────────
  test("leftRecursive: should stack overflow on left-recursive grammar") {
    assertThrows[StackOverflowError] {
      run(ParserPrograms.leftRecursive, "1+2+3")
    }
  }

  // ── indirectLeftRecursive ───────────────────────────────────────────────────
  test("indirectLeftRecursive: should stack overflow on indirect left recursion") {
    assertThrows[StackOverflowError] {
      run(ParserPrograms.indirectLeftRecursive, "1yx")
    }
  }
}

/** Runs the shared [[TokenParserTests]] against the memoizing packrat interpreter,
 * plus packrat-specific expectations the naive interpreter cannot satisfy.
 */
class PackratTokenParserTests
  extends TokenParserTests[PackratParserF[Tokens]](new PackratTransformer[Tokens](NaiveTokenParserInterpreter()) {}) {

  override protected def run[A](program: PackratParser[Tokens, A], input: String): ParserResult[Tokens, A] =
    program.parse(input.asTokens)

  // ── memoization behaviour ─────────────────────────────────────────────────
  test("PackratParser: memoizes results, invoking the underlying parser once per input") {
    var calls = 0
    val counting: Parser[Tokens, String] = input => {
      calls += 1
      Success((input, "x"))
    }
    val packrat = PackratParser(counting)
    val first = packrat.parse("a".asTokens)
    val second = packrat.parse("a".asTokens)
    assert(first == second): Unit
    assert(calls == 1)
  }

  test("PackratParser: caches results per distinct input") {
    var calls = 0
    val counting: Parser[Tokens, String] = input => {
      calls += 1
      Success((input, "x"))
    }
    val packrat = PackratParser(counting)
    packrat.parse("a".asTokens): Unit
    packrat.parse("b".asTokens): Unit
    packrat.parse("a".asTokens): Unit
    assert(calls == 2)
  }
}

/** Runs the shared [[TokenParserTests]] against the seed-growing interpreter [[SeedGrowingTokenParserInterpreter]]. */
class SeedGrowingTokenParserTests extends TokenParserTests[TokenParser](SeedGrowingTokenParserInterpreter()) {

  override protected def run[A](program: TokenParser[A], input: String): ParserResult[Tokens, A] =
    program.parse(input.asTokens)

  // ── leftRecursive ──────────────────────────────────────────────────────────
  test("leftRecursive: parses a direct left-recursive grammar") {
    run(ParserPrograms.leftRecursive, "1+2+3") match {
      case Success((remaining, parsed)) =>
        assert(parsed == "((n+n)+n)"): Unit
        assert(remaining.isEmpty)
      case Failure(e) => fail(e.getMessage)
    }
  }

  // ── indirectLeftRecursive ───────────────────────────────────────────────────
  test("indirectLeftRecursive: parses an indirect left recursion") {
    run(ParserPrograms.indirectLeftRecursive, "1xyx") match {
      case Success((remaining, parsed)) =>
        assert(parsed == "(((n)x)y)x"): Unit
        assert(remaining.isEmpty)
      case Failure(e) => fail(e.getMessage)
    }
  }
}
