import scala.collection.JavaConversions._

class ScannerError(expected: String, look: Token) extends RuntimeException (s"${look.fileName}: ${look.lineNumber} Expected: $expected Found: $look")

