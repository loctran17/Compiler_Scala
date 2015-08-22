object TokenType extends Enumeration 
{


  val LP = new TokenType()

  val RP = new TokenType()


  val IF = new TokenType()

  val THEN = new TokenType()

  val ELSE = new TokenType()

  val BEGIN = new TokenType()

  val END = new TokenType()

  val WHILE = new TokenType()

  val DO = new TokenType()

  val program = new TokenType()

  val VAR = new TokenType()

  val AS = new TokenType()

  val INT = new TokenType()

  val BOOL = new TokenType()

  val WRITEINT = new TokenType()

  val READINT = new TokenType()

  val X = new TokenType()

  val EOF = new TokenType()

  val ASGN = new TokenType()

  val NUMBER = new TokenType()

  val IDENT = new TokenType()

  val BOOLLIT = new TokenType()

  val SC = new TokenType()

  val MULTIPLICATIVE = new TokenType()

  val ADDITIVE = new TokenType()

  val COMPARE = new TokenType()

  class TokenType extends Val

  implicit def convertValue(v: Value): TokenType = v.asInstanceOf[TokenType]
}
