import Token._
import TokenType._
object Token {

  val EOF = new Token(TokenType.EOF, "EOF")
}

class Token(tokenType: TokenType, value: String)// intValue: Int, boolValue: Boolean, fileName: String, lineNumber: Int) {
{
  
//    private var tokenType: TokenType = _
//
    var intValue: Int = _
//
    var boolValue: Boolean = _
//
    var fileName: String = _
//
   var lineNumber: Int = _

  def this(tokenType: TokenType, value: String, intValue1: Int)
  {
    this(tokenType, value)
    this.intValue = intValue
    }

  def this(tokenType: TokenType, value: String, boolValue: Boolean) {
    this(tokenType, value)
    
    this.boolValue = boolValue
  }

  def getType(): TokenType = this.tokenType
  
  
  override def toString(): String = {
     if(this.tokenType.toString().equalsIgnoreCase(TokenType.NUMBER.toString()) || this.tokenType.toString().equalsIgnoreCase(TokenType.IDENT.toString()))
         this.tokenType.toString() + " : "+value;
     else if( this.tokenType.toString().equalsIgnoreCase(TokenType.BOOLLIT.toString()) ||
         this.tokenType.toString().equalsIgnoreCase(TokenType.SC.toString()) ||
         this.tokenType.toString().equalsIgnoreCase(TokenType.MULTIPLICATIVE.toString()) ||
         this.tokenType.toString().equalsIgnoreCase(TokenType.ADDITIVE.toString()) ||
         this.tokenType.toString().equalsIgnoreCase(TokenType.COMPARE.toString()) )
         value
     else
       this.tokenType.toString()
       
  }
    
  

}
