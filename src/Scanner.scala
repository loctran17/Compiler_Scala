import java.io.BufferedReader
import java.io.FileReader
import java.io.IOException
import java.util.LinkedList
import TokenType._;

class Scanner(private var fileName: String) {

  private var tokens: LinkedList[Token] = new LinkedList[Token]()

  private var lineNumber: Int = _

  def look(): Token = {
    if (tokens.isEmpty) {
      return Token.EOF
    }
    tokens.getFirst
  }

  def lookType(): TokenType = this.look().getType

  def isLookType(`type`: TokenType): Boolean = this.look().getType == `type`

  def get(): Token = {
    if (tokens.isEmpty) {
      return Token.EOF
    }
    tokens.removeFirst()
  }

  def scan() 
  {
      this.lineNumber = 0
      val reader = new BufferedReader(new FileReader(this.fileName))
      var line: String = ""
      while ({line = reader.readLine(); line!=null}) {
        println("scan: "+line)
       
      // Avoid blank lines
      if(line.trim()!="")
      {
        // Remove comments from scanner
        if(line.contains("%"))
        {
          var commentPos = line.indexOf("%");
          
          if(commentPos==0)
            commentPos=1;
          line = line.substring(0,commentPos-1);
        }
        this.lineNumber += 1
        println("line after comment removal: " + line)
        this.tokenize(line)
      }    
    }
    reader.close()
  }

  private def addLast(token: Token) {
    token.lineNumber = this.lineNumber
    println("addLast, line # "+token.lineNumber + "  token: "+ token.toString())
    this.tokens.addLast(token)
    token.fileName = this.fileName
  }

  private def tokenize(line: String) {
    
    
    println("Inside tokenize first line: '" + line + "'")

    val words = line.trim.toUpperCase().split("\\s+")
    
    for (word <- words) 
    { 
      println("Word in tokenize: '"+ word+"'")
      if (word == null) //continue
      {
        println("Checkpoint 1")
      }
      //ord = word.trim.toUpperCase
      if (word.length == 0) //continue
        println("Checkpoint 2")
      
      if (word.equalsIgnoreCase("writeInt")) {
        addLast(new Token(TokenType.WRITEINT, word))
      } else if (word.equalsIgnoreCase("READINT")) {
        addLast(new Token(TokenType.READINT, word))
      } else if (word.equalsIgnoreCase("IF")) {
        addLast(new Token(TokenType.IF, word))
      } else if (word.equalsIgnoreCase("THEN")) {
        addLast(new Token(TokenType.THEN, word))
      } else if (word.equalsIgnoreCase("ELSE")) {
        addLast(new Token(TokenType.ELSE, word))
      } else if (word.equalsIgnoreCase("begin")) {
        addLast(new Token(TokenType.BEGIN, word))
      } else if (word.equalsIgnoreCase("END")) {
        addLast(new Token(TokenType.END, word))
      } else if (word.equalsIgnoreCase("WHILE")) {
        addLast(new Token(TokenType.WHILE, word))
      } else if (word.equalsIgnoreCase("DO")) {
        addLast(new Token(TokenType.DO, word))
      } else if (word.equalsIgnoreCase("program")) {
        println("Coming in " + word)
        addLast(new Token(TokenType.program, word.toLowerCase().toString()))
      } else if (word.equalsIgnoreCase("VAR")) {
        addLast(new Token(TokenType.VAR, word.toString()))
      } else if (word.equalsIgnoreCase("as")) {
        addLast(new Token(TokenType.AS, word))
      } else if (word.equalsIgnoreCase("INT")) {
        addLast(new Token(TokenType.INT, word))
      } else if (word.equalsIgnoreCase("BOOL")) {
        addLast(new Token(TokenType.BOOL, word))
      } 
//      else if (word.equalsIgnoreCase("SQRT")) {
//        addLast(new Token(TokenType.SQRT, word))
//      } 
      else if (word.equalsIgnoreCase("(")) {
        addLast(new Token(TokenType.LP, word))
      } else if (word.equalsIgnoreCase(")")) {
        addLast(new Token(TokenType.RP, word))
      } else if (word.equalsIgnoreCase(":=")) {
        addLast(new Token(TokenType.ASGN, word))
      } else if (word.equalsIgnoreCase(";")) {
        addLast(new Token(TokenType.SC, word))
      } else if (word.equalsIgnoreCase("*") || word.equalsIgnoreCase("div" )|| word.equalsIgnoreCase("mod")) {
        addLast(new Token(TokenType.MULTIPLICATIVE, word))
      } else if (word.equalsIgnoreCase("+") || word.equalsIgnoreCase("-")) {
        addLast(new Token(TokenType.ADDITIVE, word))
      } else if (word.equalsIgnoreCase("=") || word.equalsIgnoreCase("!=") || word.equalsIgnoreCase("<") || word.equalsIgnoreCase(">") || 
        word.equalsIgnoreCase("<=") || 
        word.equalsIgnoreCase(">=")) {
        addLast(new Token(TokenType.COMPARE, word))
      } else if (word.matches("(FALSE)|(TRUE)")) {
        addLast(new Token(TokenType.BOOLLIT, word, word.equalsIgnoreCase("TRUE")))
      } else if (word.matches("((-|e)?[1-9][0-9]*)|0")) {
        addLast(new Token(TokenType.NUMBER, word, java.lang.Integer.parseInt(word)))
      } else if (word.matches("[A-Z][A-Z0-9]*")) {
        addLast(new Token(TokenType.IDENT, word))
      } else {
        System.err.println("SCANNER ERROR " + this.fileName + ":" + this.lineNumber + 
          " Unknown token : '" + 
          word + 
          "'")
      }
      println("Checkpoint 3")

    }
  }
}
