package neocheck
package parser

import scala.Tuple.Size
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

  override def recursive[Outputs <: Tuple](p: Tuple.Map[Outputs, ParserF[Input]] => Tuple.Map[Outputs, ParserF[Input]])
                                          (using size: ValueOf[Size[Outputs]]): Tuple.Map[Outputs, ParserF[Input]] = {
    type ParserResultF = [Output] =>> ParserResult[Input, Output]

    def parse(parsers: Tuple.Map[Outputs, ParserF[Input]], input: Input): Tuple.Map[Outputs, ParserResultF] = {
      type Result[T] = T match {
        case Parser[Input, output] => ParserResult[Input, output]
      }

      def result[T](t: T): Result[T] = {
        (t.asMatchable: @unchecked) match {
          case parser: Parser[Input, output] => parser.parse(input)
        }
      }

      parsers.map([T] => (t: T) => result(t)).asInstanceOf[Tuple.Map[Outputs, ParserResultF]]
    }

    def improved(best: Tuple.Map[Outputs, ParserResultF], result: Tuple.Map[Outputs, ParserResultF]): Boolean = {
      def matchMap[T](t: T): Boolean = {
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
    def loop(current: Tuple.Map[Outputs, ParserF[Input]], best: Tuple.Map[Outputs, ParserResultF])(input: Input): Tuple.Map[Outputs, ParserResultF] = {
      val next = p(current)
      val result = parse(next, input)
      if improved(best, result) then loop(next, result)(input) else best
    }

    val seeds = Tuple.fromArray(Array.fill(size.value)(failure("left recursion seed")))
      .asInstanceOf[Tuple.Map[Outputs, ParserF[Input]]]

    val parsers = Array.tabulate[Parser[Input, ?]](size.value) {
      i => {
        input => loop(seeds, parse(seeds, input))(input).productElement(i).asInstanceOf[ParserResult[Input, Any]]
      }
    }
    Tuple.fromArray(parsers).asInstanceOf[Tuple.Map[Outputs, ParserF[Input]]]
  }
}
