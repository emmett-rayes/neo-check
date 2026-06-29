package neocheck
package parser

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.{Failure, Success}

/** A kleene token interpreter that counts how many times its primitive parsers are actually run.
 *
 * The counters observe work performed by the underlying `literal`/`regex` parsers.
 * When this interpreter is wrapped by a [[PackratTransformer]], the memoization layer sits above these primitives,
 * so the counters reveal exactly how much redundant primitive work the packrat memo eliminates during kleene iteration.
 */
private final class CountingKleeneInterpreter
  extends TokenParserInterpreter, KleeneRecursionInterpreter[Tokens] {

  var literalParses = 0
  var regexParses = 0
  val literalParsesByExpected: mutable.Map[String, Int] = mutable.Map.empty.withDefaultValue(0)

  override def literal(expected: String): TokenParser[String] = {
    val p = super.literal(expected)
    input => {
      literalParses += 1
      literalParsesByExpected(expected) += 1
      p.parse(input)
    }
  }

  override def regex(expected: Regex): TokenParser[String] = {
    val p = super.regex(expected)
    input => {
      regexParses += 1
      p.parse(input)
    }
  }
}

/** Unit tests for the memoization behaviour of [[PackratParser]] and the [[PackratTransformer]],
 * both on their own and layered on top of the kleene interpreter.
 */
class PackratParserTests extends AnyFunSuite {

  // ── PackratParser memoization ─────────────────────────────────────────────

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

  // ── kleene packrat memoization ──────────────────────────────────────

  test("KleenePackratParser: re-parsing the same input reuses the grown result without any further primitive work") {
    val underlying = CountingKleeneInterpreter()
    val packrat = new PackratTransformer[Tokens](underlying) {}
    val program = ParserPrograms.leftRecursive(using packrat)

    val first = program.parse("1+2+3".asTokens)
    val regexAfterFirst = underlying.regexParses
    val literalAfterFirst = underlying.literalParses

    val second = program.parse("1+2+3".asTokens)

    assert(first == second): Unit
    assert(underlying.regexParses == regexAfterFirst): Unit
    assert(underlying.literalParses == literalAfterFirst)
  }

  test("KleenePackratParser: memoization eliminates the redundant sub-parses that kleene iteration would otherwise repeat") {
    val plain = CountingKleeneInterpreter()
    ParserPrograms.leftRecursive(using plain).parse("1+2+3".asTokens): Unit

    val underlying = CountingKleeneInterpreter()
    val packrat = new PackratTransformer[Tokens](underlying) {}
    ParserPrograms.leftRecursive(using packrat).parse("1+2+3".asTokens): Unit

    assert(underlying.regexParses < plain.regexParses): Unit
    assert(underlying.literalParses < plain.literalParses)
  }

  test("KleenePackratParser: each input position is parsed once despite kleene iteration loop re-running the grammar") {
    val underlying = CountingKleeneInterpreter()
    val packrat = new PackratTransformer[Tokens](underlying) {}
    ParserPrograms.leftRecursive(using packrat).parse("1+2+3".asTokens): Unit

    // "1+2+3" has three numeric terms; memoization means the term parser runs exactly once per position,
    // even though kleene interation applies the grammar repeatedly to grow the left-recursive result.
    assert(underlying.regexParses == 3)
  }

  test("KleenePackratParser: distinct inputs are grown and memoized independently") {
    val underlying = CountingKleeneInterpreter()
    val packrat = new PackratTransformer[Tokens](underlying) {}
    val program = ParserPrograms.leftRecursive(using packrat)

    program.parse("1+2+3".asTokens) match {
      case Success((remaining, parsed)) =>
        assert(parsed == "((n+n)+n)"): Unit
        assert(remaining.isEmpty): Unit
      case Failure(e) => fail(e.getMessage)
    }

    val regexAfterFirstInput = underlying.regexParses

    program.parse("4+5".asTokens) match {
      case Success((remaining, parsed)) =>
        assert(parsed == "(n+n)"): Unit
        assert(remaining.isEmpty): Unit
      case Failure(e) => fail(e.getMessage)
    }

    // A new input is not served from the first input's cache: it triggers fresh primitive work.
    assert(underlying.regexParses > regexAfterFirstInput)
  }

  test("KleenePackratParser: the inner parser 'b' of indirectLeftRecursive is not memoized") {
    val underlying = CountingKleeneInterpreter()
    val packrat = new PackratTransformer[Tokens](underlying) {}

    ParserPrograms.indirectLeftRecursive(using packrat).parse("1xyx".asTokens) match {
      case Success((remaining, parsed)) =>
        assert(parsed == "(((n)x)y)x"): Unit
        assert(remaining.isEmpty): Unit
      case Failure(e) => fail(e.getMessage)
    }

    // `term` is built once, outside `P.recursive`, so the packrat layer memoizes it: the single numeric
    // term in "1xyx" is parsed exactly once across the whole kleene iteration process.
    assert(underlying.regexParses == 1): Unit

    // `b` is rebuilt via host-language recursion on every growth iteration (`val b = ...` inside the body),
    // so each iteration wraps it in a fresh, empty memo.
    // Its distinguishing literal "y" is therefore re-parsed at the same position across iterations
    // instead of being served from a cache, even though every parser the algebra constructs is wrapped for memoization.
    assert(underlying.literalParsesByExpected("y") > 1)
  }

  test("KleenePackratParser: the parsers 'a' and 'b' of indirectLeftRecursiveTuple are memoized") {
    val plain = CountingKleeneInterpreter()
    val plainParsers = ParserPrograms.indirectLeftRecursiveTuple(using plain)
    plainParsers.a.parse("1xyx".asTokens): Unit
    plainParsers.b.parse("1xyx".asTokens): Unit

    val underlying = CountingKleeneInterpreter()
    val packrat = new PackratTransformer[Tokens](underlying) {}
    val packratParsers = ParserPrograms.indirectLeftRecursiveTuple(using packrat)
    packratParsers.a.parse("1xyx".asTokens): Unit
    packratParsers.b.parse("1xyx".asTokens): Unit

    // Unlike `indirectLeftRecursive`, the tuple form defines both `a` and `b` as top-level recursion outputs with
    // stable identities, so the packrat layer can memoize either one.
    assert(underlying.regexParses < plain.regexParses): Unit
    assert(underlying.literalParses < plain.literalParses)
  }
}
