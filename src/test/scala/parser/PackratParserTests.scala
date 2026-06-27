package neocheck
package parser

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.{Failure, Success}

/** Length-based ordering on [[Tokens]], required to mix in [[SeedGrowingRecursionInterpreter]]. */
private given Ordering[Tokens] = Ordering.by(_.length)

/** A seed-growing token interpreter that counts how many times its primitive parsers are actually run.
 *
 * The counters observe work performed by the *underlying* `literal`/`regex` parsers. When this interpreter
 * is wrapped by a [[PackratTransformer]], the memoization layer sits above these primitives, so the counters
 * reveal exactly how much redundant primitive work the packrat memo eliminates during seed growing.
 */
private final class CountingSeedGrowingInterpreter
  extends TokenParserInterpreter, SeedGrowingRecursionInterpreter[Tokens] {

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
 * both on their own and layered on top of the seed-growing interpreter.
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
}
