
class ParserError(expected: String, look: Token) extends RuntimeException(look.fileName + ":" + look.lineNumber + " Expected: " + 
  expected + 
  " Found: " + 
  look)
