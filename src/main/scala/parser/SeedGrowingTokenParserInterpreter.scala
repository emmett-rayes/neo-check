package neocheck
package parser

private given Ordering[Tokens] = Ordering.by(_.length)

final class SeedGrowingTokenParserInterpreter extends TokenParserInterpreter, SeedGrowingRecursionInterpreter[Tokens]
