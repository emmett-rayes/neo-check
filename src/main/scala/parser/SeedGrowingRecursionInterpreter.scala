package neocheck
package parser

import scala.compiletime.asMatchable
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.{Failure, Success}

/** A partial implementation of an interpreter of [[ParserAlgebra]] for [[Parser]].
 *
 * Uses the seed growing technique to implement the `recursive` combinator, which allows for direct left-recursion.
 *
 * @tparam Input the type of the input to be parsed.
 */
trait SeedGrowingRecursionInterpreter[Input: Ordering] extends ParserAlgebra[ParserF[Input]] {
  override def recursive[Output](p: Parser[Input, Output] => Parser[Input, Output]): Parser[Input, Output] = {

    def improved(best: ParserResult[Input, Output], result: ParserResult[Input, Output]): Boolean = {
      (best, result) match {
        case (Failure(_), Success(_)) => true
        case (Success(r1), Success(r2)) if r2.remaining < r1.remaining => true
        case _ => false
      }
    }

    @annotation.tailrec
    def loop(current: Parser[Input, Output], best: ParserResult[Input, Output])(input: Input): ParserResult[Input, Output] = {
      val next = p(current)
      val result = next.parse(input)
      if improved(best, result) then loop(next, result)(input) else best
    }

    val seed = failure("left recursion seed")
    input => loop(seed, seed.parse(input))(input)
  }

  override def recursive[Outputs <: NamedTuple.AnyNamedTuple]
                        (p: NamedTuple.Map[Outputs, ParserF[Input]] => NamedTuple.Map[Outputs, ParserF[Input]])
                        (using size: ValueOf[NamedTuple.Size[Outputs]]): NamedTuple.Map[Outputs, ParserF[Input]] = {
    type ParserResultF[I] = [Output] =>> ParserResult[I, Output]

    def parse(parsers: NamedTuple.Map[Outputs, ParserF[Input]],
              input: Input): NamedTuple.Map[Outputs, ParserResultF[Input]] = {
      type Result[T] = T match {
        case Parser[Input, output] => ParserResult[Input, output]
      }

      def result[T](t: T): Result[T] = {
        (t.asMatchable: @unchecked) match {
          case parser: Parser[Input, output] => parser.parse(input)
        }
      }

      // cast safety:
      // `ParserF` in T makes `Result[T]` produce `ParserResultF[Input]`
      // `Outputs` in T is the same as `Outputs` in the cast type
      parsers.map([T] => (t: T) => result(t)).asInstanceOf[NamedTuple.Map[Outputs, ParserResultF[Input]]]
    }

    def improved(best: NamedTuple.Map[Outputs, ParserResultF[Input]],
                 result: NamedTuple.Map[Outputs, ParserResultF[Input]]): Boolean = {
      def matchMap[T](t: T): Boolean = {
        // cast safety:
        // `matchMap` is only applied to `best.zip(result)`
        // `best` and `result` contain `ParserResult[Input, Any]` at every position
        val (best, result) = t.asInstanceOf[(ParserResult[Input, Any], ParserResult[Input, Any])]
        (best, result) match {
          case (Failure(_), Success(_)) => true
          case (Success(r1), Success(r2)) => r2.remaining < r1.remaining
          case _ => false
        }
      }

      best.zip(result).map[[_] =>> Boolean]([T] => (t: T) => matchMap(t)).toList.contains(true)
    }

    @annotation.tailrec
    def loop(current: NamedTuple.Map[Outputs, ParserF[Input]], best: NamedTuple.Map[Outputs, ParserResultF[Input]])
            (input: Input): NamedTuple.Map[Outputs, ParserResultF[Input]] = {
      val next = p(current)
      val result = parse(next, input)
      if improved(best, result) then loop(next, result)(input) else best
    }

    // cast safety:
    // `seeds` contains `Parser[Input, Output(i)]` at every position `i`
    val seed = Tuple.fromArray(Array.fill(size.value)(failure("left recursion seed")))
      .asInstanceOf[NamedTuple.Map[Outputs, ParserF[Input]]]

    val parsers = Array.tabulate[Parser[Input, ?]](size.value) {
      i => {
        // cast safety:
        // `loop` produces `Map[Outputs, ParserResultF[Input]]` which has `ParserResult[Input, _)]` at every position
        input => loop(seed, parse(seed, input))(input).toTuple.productElement(i).asInstanceOf[ParserResult[Input, Any]]
      }
    }
    // cast safety:
    // `parsers` contains `ParserF[Input, Outputs(i)]` at every position `i`
    Tuple.fromArray(parsers).asInstanceOf[NamedTuple.Map[Outputs, ParserF[Input]]]
  }
}
