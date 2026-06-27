package neocheck
package parser

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
    val seed = this.failure("left recursion seed")
    input => {
      @annotation.tailrec
      def loop(current: Parser[Input, Output], best: ParserResult[Input, Output]): ParserResult[Input, Output] = {
        val next = p(current)
        val result = next.parse(input)
        (best, result) match {
          case (Failure(_), Success(_)) => loop(next, result)
          case (Success(r1), Success(r2)) if r2.remaining < r1.remaining => loop(next, result)
          case _ => best
        }
      }

      loop(seed, seed.parse(input))
    }
  }
}
