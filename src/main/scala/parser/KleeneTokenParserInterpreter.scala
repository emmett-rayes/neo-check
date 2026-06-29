package neocheck
package parser

private given Ordering[Tokens] = Ordering.by(_.length)

final class KleeneTokenParserInterpreter extends TokenParserInterpreter, KleeneRecursionInterpreter[Tokens]
