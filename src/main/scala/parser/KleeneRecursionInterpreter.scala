package neocheck
package parser

import scala.compiletime.asMatchable
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.{Failure, Success}

object KleeneRecursionInterpreter {
  def distance[Input: Ordering, Output](best: ParserResult[Input, Output], result: ParserResult[Input, Output]): Int = {
    (best, result) match {
      case (Failure(_), Failure(_)) => 0
      case (Failure(_), Success(_)) => 1
      case (Success(_), Failure(_)) => -1
      case (Success(r1), Success(r2)) =>
        if r2.remaining == r1.remaining then 0 else if r2.remaining < r1.remaining then 1 else -1
    }
  }
}

/** A partial implementation of an interpreter of [[ParserAlgebra]] for [[Parser]].
 *
 * Uses kleene iteration to implement the `recursive` combinator, which allows for direct left-recursion.
 *
 * @tparam Input the type of the input to be parsed.
 */
trait KleeneRecursionInterpreter[Input: Ordering] extends ParserAlgebra[ParserF[Input]] {
  override def recursive[Output](p: Parser[Input, Output] => Parser[Input, Output]): Parser[Input, Output] = {
    def improved(best: ParserResult[Input, Output], result: ParserResult[Input, Output]): Boolean = {
      KleeneRecursionInterpreter.distance(best, result) > 0
    }

    @annotation.tailrec
    def loop(current: Parser[Input, Output], best: ParserResult[Input, Output])(input: Input): ParserResult[Input, Output] = {
      val next = p(current)
      val result = next.parse(input)
      if improved(best, result) then loop(next, result)(input) else best
    }

    val seed = failure("Infinite recursion detected")
    input => {
      // this priming is necessary to make sure that the result of parsing with´seed
      // is always less than the result of parsing with the first parser produced by `p`
      val first = p(seed)
      loop(first, first.parse(input))(input)
    }
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
      def distance[T](t: T): Int = {
        // cast safety:
        // `distance` is only applied to `best.zip(result)`
        // `best` and `result` contain `ParserResult[Input, Any]` at every position
        val (best, result) = t.asInstanceOf[(ParserResult[Input, Any], ParserResult[Input, Any])]
        KleeneRecursionInterpreter.distance(best, result)
      }

      val distances = best.zip(result).map[[_] =>> Int]([T] => (t: T) => distance(t)).toList
      distances.contains(1) && !distances.contains(-1)
    }

    @annotation.tailrec
    def loop(current: NamedTuple.Map[Outputs, ParserF[Input]], best: NamedTuple.Map[Outputs, ParserResultF[Input]])
            (input: Input): NamedTuple.Map[Outputs, ParserResultF[Input]] = {
      val next = p(current)
      val result = parse(next, input)
      if improved(best, result) then loop(next, result)(input) else best
    }

    // cast safety:
    // `seed` contains `Parser[Input, Output(i)]` at every position `i`
    val seed = Tuple.fromArray(Array.fill(size.value)(failure("left-recursion seed")))
      .asInstanceOf[NamedTuple.Map[Outputs, ParserF[Input]]]

    val parsers = Array.tabulate[Parser[Input, ?]](size.value) {
      i => {
        input => {
          // this priming is necessary to make sure that the result of parsing with´seed
          // is always less than the result of parsing with the first parser produced by `p`
          val first = p(seed)
          // cast safety:
          // `loop` produces `Map[Outputs, ParserResultF[Input]]` which has `ParserResult[Input, _)]` at every position
          loop(first, parse(first, input))(input).toTuple.productElement(i).asInstanceOf[ParserResult[Input, Any]]
        }
      }
    }
    // cast safety:
    // `parsers` contains `ParserF[Input, Outputs(i)]` at every position `i`
    Tuple.fromArray(parsers).asInstanceOf[NamedTuple.Map[Outputs, ParserF[Input]]]
  }
}
