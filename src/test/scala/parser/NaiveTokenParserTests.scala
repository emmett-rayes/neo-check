package neocheck
package parser

import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}

/** Unit tests for [[NaiveTokenParserInterpreter]] via the [[ParserPrograms]] catalogue. */
class NaiveTokenParserTests extends AnyFunSuite {

  given NaiveTokenParserInterpreter = NaiveTokenParserInterpreter()

  /** Convenience runner: converts a String to [[Tokens]] and drives the parser. */
  private def run[A](program: TokenParser[A], input: String) =
    program.parse(input.asTokens)

  // ── leftRecursive ──────────────────────────────────────────────────────────
  test("leftRecursive: should stack overflow on left-recursive grammar") {
    assertThrows[StackOverflowError] {
      val program = ParserPrograms.leftRecursive
      val input = "1+2+3"
      run(program, input)
    }
  }

  // ── rightRecursive ─────────────────────────────────────────────────────────
  test("rightRecursive: should parse right-recursive grammar") {
    val program = ParserPrograms.rightRecursive
    val input = "1+2+3"
    run(program, input) match {
      case Success((remaining, parsed)) =>
        assert(parsed == "(n+(n+n))"): Unit
        assert(remaining.isEmpty)
      case Failure(e) => fail(e.getMessage)
    }
  }
}
