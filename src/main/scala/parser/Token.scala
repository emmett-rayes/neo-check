package neocheck
package parser

/** A token is a single character of the input to be parsed. */
type Token = Char

/** A sequence of tokens representing the input to be parsed. */
type Tokens = IndexedSeq[Token]

/** Operations for working with sequences of tokens. */
extension (tokens: Tokens) {

  /** Drops any leading whitespace tokens from this sequence.
   *
   * @return the remaining tokens after the leading whitespace has been removed.
   */
  def skipWhitespace: Tokens = tokens.span(_.isWhitespace)._2
}

/** Operations for working with strings as tokens. */
extension (self: String) {

  /** Converts this string into its sequence of tokens.
   *
   * @return the sequence of tokens representing this string.
   */
  def asTokens: Tokens = self
}
