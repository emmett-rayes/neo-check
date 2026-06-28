package neocheck
package parser

object ParserCombinators {
  /** Derived combinators available on any parser of a [[ParserAlgebra]].
   *
   * These build on the primitive operations of the algebra (`flatMap`,
   * `orElse`, `success`, `failure`), so they work for any `Parser` for which a
   * [[ParserAlgebra]] instance is in scope.
   *
   * @param self the parser to extend.
   * @tparam Parser the higher-kinded type of the parser.
   * @tparam Output the result type produced by `self`.
   */
  extension [Parser[_], Output](using P: ParserAlgebra[Parser])(self: Parser[Output]) {
    /** Transforms the result of `self` by applying `f` to its output.
     *
     * Runs `self` and, if it succeeds, applies `f` to the produced value; if
     * `self` fails, the failure is propagated unchanged.
     *
     * @param f the function applied to the output of `self`.
     * @tparam Mapped the result type produced by `f`.
     * @return a parser that runs `self`, then maps its output with `f`.
     */
    def map[Mapped](f: Output => Mapped): Parser[Mapped] = {
      self.flatMap { a => P.success(f(a)) }
    }

    /** Tries to apply `self`, returning `Some` of its output if it succeeds, or `None` if it fails.
     *
     * @return a parser that produces an `Option` of the output of `self`.
     */
    def optional: Parser[Option[Output]] = {
      self.map(Some(_)).orElse(P.success(None))
    }

    /** Applies `self` greedily as many times as possible, collecting the results into a list.
     *
     * Repeatedly runs `self`, accumulating each output, until it no longer succeeds,
     * then succeeds with the collected list. Matching zero times is allowed and yields an empty list,
     * so this parser never fails.
     *
     * @return a parser producing the list of outputs of every successful
     *         application of `self`.
     */
    def repeated: Parser[List[Output]] = {
      val greedy =
        for
          selfOutput <- self
          otherOutputs <- self.repeated
        yield selfOutput :: otherOutputs
      greedy.orElse(P.success(List()))
    }

    /** Applies `self` greedily, requiring at least `n` successful applications.
     *
     * Behaves like [[repeated]] but fails if `self` matches fewer than `n` times.
     *
     * @param n the minimum number of successful applications required.
     * @return a parser producing the list of outputs, or a failure if fewer
     *         than `n` applications succeed.
     */
    def atLeast(n: Int): Parser[List[Output]] = {
      self.repeated.flatMap { outputs =>
        if outputs.size >= n then P.success(outputs)
        else P.failure(s"Expected at least ${n - outputs.size} more element(s) after ${outputs.mkString}.")
      }
    }

    /** Sequences `self` with `other`, pairing both results.
     *
     * Runs `self`, then `other` on the remaining input, and succeeds with a tuple of their outputs;
     * if either parser fails, the failure is propagated.
     *
     * @param other the parser run after `self`.
     * @tparam Then the result type of `other`.
     * @return a parser producing the outputs of `self` and `other` as a tuple.
     */
    def andThen[Then](other: Parser[Then]): Parser[(Output, Then)] = {
      for
        selfOutput <- self
        otherOutput <- other
      yield (selfOutput, otherOutput)
    }

    /** Sequences `self` with `other`, keeping only the result of `other`.
     *
     * Runs `self`, discards its output, then runs `other,` and yields its result.
     *
     * @param other the parser whose result is kept.
     * @tparam Then the result type of `other`.
     * @return a parser producing the output of `other`.
     */
    def skipThen[Then](other: Parser[Then]): Parser[Then] = {
      self.andThen(other).map(_._2)
    }

    /** Sequences `self` with `other`, keeping only the result of `self`.
     *
     * Runs `self`, then runs `other,` and discards its output, yielding the
     * result of `self`. Useful for consuming a trailing delimiter or suffix.
     *
     * @param other the parser whose result is discarded.
     * @tparam Skip the result type of `other`.
     * @return a parser producing the output of `self`.
     */
    def thenSkip[Skip](other: Parser[Skip]): Parser[Output] = {
      self.andThen(other).map(_._1)
    }

    /** Runs `self` surrounded by `first` and `second`, keeping only its result.
     *
     * Runs `first`, then `self`, then `second`, discarding the outputs of `first` and `second`.
     *
     * @param first  the parser run before `self`.
     * @param second the parser run after `self`.
     * @tparam First  the result type of `first`.
     * @tparam Second the result type of `second`.
     * @return a parser producing the output of `self`.
     */
    def between[First, Second](first: Parser[First], second: Parser[Second]): Parser[Output] = {
      first.skipThen(self).thenSkip(second)
    }

    /** Runs `self` repeatedly, creating a left-associated result using the binary operator `op`.
     *
     * @param op the binary operator used to combine results.
     * @param z  the initial value to use for the leftmost combination.
     * @return a parser producing the left-associated result of applying `op` to the outputs of `self`.
     */
    def chainLeft(z: Output)(op: (Output, Output) => Output): Parser[Output] = {
      self.repeated.map { output => output.foldLeft(z)(op) }
    }

    /** Runs `self` repeatedly, creating a right-associated result using the binary operator `op`.
     *
     * @param op the binary operator used to combine results.
     * @param z  the initial value to use for the rightmost combination.
     * @return a parser producing the right-associated result of applying `op` to the outputs of `self`.
     */
    def chainRight(z: Output)(op: (Output, Output) => Output): Parser[Output] = {
      self.repeated.map { output => output.foldRight(z)(op) }
    }
  }
}
