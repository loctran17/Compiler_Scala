import java.io._
import Node._;

import scala.collection.JavaConversions._
import TokenType._;

object TypeCheckParser {
  var myVar: Int = 1
  var isProgram: Boolean = false 
  var isDeclList: Boolean = false
  var isStmtList: Boolean = false
  var varStmtList: Int = 0
  var prevVarStmtList: Int = 0
  var symbolTable: Map[String, Int] = Map()
  def toDotFile(fileNameAST: String, node: Node): Int = {
    val streamAST = new PrintStream(new FileOutputStream(fileNameAST))
    streamAST.println("digraph tl12Ast {")
    streamAST.println("ordering=out;")
    streamAST.println("node [shape = box, style = filled];")
    //printAST(streamAST, node, node, 1)
    var returnType = printTypeChecking(streamAST, node, 1)
    streamAST.println("}")
    return returnType
  }
  
  // Build Symbol Table here 
  private def printTypeCheckingDeclarations(stream: PrintStream, event: Any, n: Int): Int =
  {
    
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    
    if (c != Node.E) {
      // "VAR" "INDENT : X" "AS" "TYPE" ";"
      c = node.getChildren.get(1)
      
      // Varible name
      var varible: String = c.toString().substring(8, c.toString().length())
      println("printTypeCheckingStatementSequence variable = " + varible)
      
      // Type 
      c =  node.getChildren.get(3)
      var nodeType:Node = c.asInstanceOf[Node]
      var declType: Int = 0
      var cc:Any = nodeType.getChildren.get(0)
      println("printTypeCheckingStatementSequence: ",n + ", event: " + cc.toString())
      if (cc.toString() == "INT") declType = 3
      else if (cc.toString() == "BOOL") declType = 2
      else declType = 1
      
      println("printTypeCheckingStatementSequence declType = " + declType)
      
      myVar = n+1
      stream.println("\tn" + myVar + " [label=\"decl: \'" + varible + "\'\",fillcolor=\"/pastel13/" + declType +  "\",shape=box]")
      stream.println("\tn2 -> n" + myVar)
      
      myVar = myVar + 1
      stream.println("\tn" + myVar + " [label=\"" + cc.toString().toLowerCase() + "\",fillcolor=\"/x11/lightgrey\",shape=box]")
      stream.println("\tn" + (myVar - 1) + " -> n" + myVar)
      
      symbolTable += (varible -> declType )
      
      // Declaration
      c = node.getChildren.get(5)
      printTypeCheckingDeclarations(stream, c, n+1)
    }
    
    
    return 0
  }
  
  
  private def printTypeCheckingAssignment(stream: PrintStream, event: Any, n: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    for (c <- node.getChildren) 
    {
      println("printTypeCheckingAssignment: " + n + ", event: " + c.toString())
    }
    
    // INDENT : X" ASIGN "READINT/EXPRESSION"
    c = node.getChildren.get(0)
    var varible: String = c.toString().substring(8, c.toString().length())
    println("printTypeCheckingStatementSequence variable = " + varible)
    var declType = symbolTable.get(varible)
    var returnType: Int = 0
    
    c= node.getChildren.get(2)
    if (c.toString() == "READINT") {
      myVar = myVar + 1
      if (declType.get == 3) { 
        stream.println("\tn"+ myVar + " [label=\":= readInt\",fillcolor=\"/pastel13/3\",shape=box]")
        returnType = 3
      } else {
        stream.println("\tn"+ myVar + " [label=\":= readInt\",fillcolor=\"/pastel13/1\",shape=box]")
        returnType = 1
      }
      stream.println("\tn" + n + " -> n" + myVar)
      myVar = myVar + 1
      stream.println("\tn"+ myVar + " [label=\""+ varible + "\",fillcolor=\"/pastel13/" + declType.get +"\",shape=box]")
      stream.println("\tn" + (myVar-1) + " -> n" + myVar)
      return returnType
      } 
    else {
      myVar = myVar + 1
      var temVal : Int = myVar
      returnType = printTypeCheckingExpression(stream, c, n)
      if (returnType == declType.get) {
        stream.println("\tn"+ temVal + " [label=\":=\",fillcolor=\"/pastel13/" + returnType + "\",shape=box]")
      } else { 
        returnType = 1
        stream.println("\tn"+ temVal + " [label=\":=\",fillcolor=\"/pastel13/" + returnType + "\",shape=box]")
      }
      stream.println("\tn" + n + " -> n" + temVal)
      myVar = myVar + 1
      stream.println("\tn"+ myVar + " [label=\""+ varible + "\",fillcolor=\"/pastel13/" + declType.get +"\",shape=box]")
      stream.println("\tn" + (myVar-1) + " -> n" + myVar)
      return returnType
    }
    
  }
  
  private def printTypeCheckingFactor(stream: PrintStream, event: Any, n: Int): Int =
  {
    println("printTypeCheckingFactor: " + n + ", event: " + event.toString())
    if (event.isInstanceOf[Node]) {
      var node:Node = event.asInstanceOf[Node]
      var c:Any = node.getChildren.get(0)
      
      if (node.getChildren.size() == 1) {
         if (c.toString().length() > 8 && c.toString().substring(0, 8) == "NUMBER :" ) {
            myVar = myVar + 1 
            stream.println("\tn" + myVar + " [label=\"" + 
                c.toString().substring(9, c.toString().length()) + "\",fillcolor=\"/pastel13/3\",shape=box]")
            stream.println("\tn" + n + " -> " + "n" + myVar)    
            println("\tn" + n + " -> " + "n" + myVar)
            
            return 3
         }
         if (c.toString().length() > 7 && c.toString().substring(0, 7) == "IDENT :") {
              myVar = myVar + 1
              var variable: String = c.toString().substring(8, c.toString().length())
              var declType = symbolTable.get(variable) 
              stream.println("\tn" + myVar + " [label=\"" + variable 
                      + "\",fillcolor=\"/pastel13/" + declType.get + "\",shape=box]")
              stream.println("\tn" + n + " -> " + "n" + myVar)
              
              return declType.get
            }
      } else {
        
      }
      for (c <- node.getChildren) 
      {
        println("printTypeCheckingFactor: " + n + ", event: " + c.toString())
      }
    }
    else {
      if (event.toString().length() > 8 && event.toString().substring(0, 8) == "NUMBER :" ) {
            myVar = myVar + 1 
            stream.println("\tn" + myVar + " [label=\"" + 
                event.toString().substring(9, event.toString().length()) + "\",fillcolor=\"/pastel13/3\",shape=box]")
            stream.println("\tn" + n + " -> " + "n" + myVar)    
            println("\tn" + n + " -> " + "n" + myVar)
            
            return 3
         }
         if (event.toString().length() > 7 && event.toString().substring(0, 7) == "IDENT :") {
              myVar = myVar + 1
              var variable: String = event.toString().substring(8, event.toString().length())
              var declType = symbolTable.get(variable) 
              stream.println("\tn" + myVar + " [label=\"" + variable 
                      + "\",fillcolor=\"/pastel13/" + declType.get + "\",shape=box]")
              stream.println("\tn" + n + " -> " + "n" + myVar)
              
              return declType.get
            }
    }  
    
    return 0
  }
  
  private def printTypeCheckingTerm(stream: PrintStream, event: Any, n: Int): Int =
  {
   if (event.isInstanceOf[Node])  {
      var node:Node = event.asInstanceOf[Node]
      var c:Any = node.getChildren.get(0)
      var returnType: Int = 0
      
      for (c <- node.getChildren) 
      {
        println("printTypeCheckingTerm: " + n + ", event: " + c.toString())
      }
      // factor
      if (node.getChildren.size() == 1) {
        printTypeCheckingFactor(stream, c, n)
      } else {
        myVar = myVar+1
        var temVal: Int = myVar
        c = node.getChildren.get(0)
        var factorType  = printTypeCheckingFactor(stream, c, temVal)
        c = node.getChildren.get(2)
        var termType = printTypeCheckingFactor(stream, c, temVal)
        c = node.getChildren.get(1)
        if (factorType == 3 && termType == 3) {
          stream.println("\tn"+ temVal + " [label=\""+ c.toString() + "\",fillcolor=\"/pastel13/2\",shape=box]")
          returnType = 3
        }
        else {
          stream.println("\tn"+ temVal + " [label=\""+ c.toString() + "\",fillcolor=\"/pastel13/1\",shape=box]")
          returnType = 1
        }
        stream.println("\tn" + n + " -> n" + temVal)
        return returnType
      } 
    } else {
      return printTypeCheckingFactor(stream, event, n)
    }
  }
  
  private def printTypeCheckingSimpleExpression(stream: PrintStream, event: Any, n: Int): Int =
  {
    if (event.isInstanceOf[Node]) {
      var node:Node = event.asInstanceOf[Node]
      var c:Any = node.getChildren.get(0)
      
      for (c <- node.getChildren) 
      {
        println("printTypeCheckingSimpleExpression: " + n + ", event: " + c.toString())
      }
      // TERM
      if (node.getChildren.size() == 1) {
        return printTypeCheckingTerm(stream, c, n)
      } else {
        
        myVar = myVar + 1
        var temVal: Int = myVar
        c = node.getChildren.get(0)
        var simpleReturn = printTypeCheckingSimpleExpression(stream, c, myVar)
        c = node.getChildren.get(2)
        var epxrReturn = printTypeCheckingExpression(stream, c, myVar)
        c = node.getChildren.get(1)
        if (simpleReturn == 3 && epxrReturn == 3) {
          stream.println("\tn"+ myVar + " [label=\""+ c.toString() + "\",fillcolor=\"/pastel13/3\",shape=box]")
          stream.println("\tn" + n + " -> n" + myVar)
          return 3
        } else {
          stream.println("\tn"+ temVal + " [label=\""+ c.toString() + "\",fillcolor=\"/pastel13/1\",shape=box]")
          stream.println("\tn" + n + " -> n" + temVal)
          return 1
        }
      } 
    } else {
      return printTypeCheckingTerm(stream, event, n)
    } 
      
      
    
    return 0
  }
  
  private def printTypeCheckingExpression(stream: PrintStream, event: Any, n: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    
    var c:Any = node.getChildren.get(0)
    
    for (c <- node.getChildren) 
    {
      println("printTypeCheckingExpression: " + n + ", event: " + c.toString())
    }
    // <simpleExpression> case 
    if (node.getChildren.size() == 1) {
      return printTypeCheckingSimpleExpression(stream, c, n)
    } else {
      // Add Compare to the tree, must be boolean
      
      myVar = myVar + 1
      var temVal: Int = myVar
      c = node.getChildren.get(0)
      var simpleReturn = printTypeCheckingSimpleExpression(stream, c, myVar)
      c = node.getChildren.get(2)
      var exprReturn = printTypeCheckingExpression(stream, c, myVar)
      c = node.getChildren.get(1)
      if (simpleReturn == 3 && exprReturn == 3) {
        stream.println("\tn"+ temVal + " [label=\""+ c.toString() + "\",fillcolor=\"/pastel13/2\",shape=box]")
        stream.println("\tn" + n + " -> n" + temVal)
        return 2
      } else {
        stream.println("\tn"+ temVal + " [label=\""+ c.toString() + "\",fillcolor=\"/pastel13/1\",shape=box]")
        stream.println("\tn" + n + " -> n" + temVal)
        return 1
      }
      
    }
      
    
    return 0  
  }
  
  private def printTypeCheckingWriteInt(stream: PrintStream, event: Any, n: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    for (c <- node.getChildren) 
    {
      println("printTypeCheckingWriteInt: " + n + ", event: " + c.toString())
    }
    myVar = myVar + 1
    var temVal: Int = myVar
    
    c = node.getChildren.get(1)
    var exprReturn = printTypeCheckingExpression(stream, c, myVar)
    if (exprReturn == 3) {
      stream.println("\tn"+ temVal + " [label=\"writeInt\",fillcolor=\"/pastel13/3\",shape=box]")
      stream.println("\tn" + n + " -> n" + temVal)
      return 3
    } else {
      stream.println("\tn"+ temVal + " [label=\"writeInt\",fillcolor=\"/pastel13/1\",shape=box]")
      stream.println("\tn" + n + " -> n" + temVal)
      return 1
    }
      
  }
  
  private def printTypeCheckingWhileStatement(stream: PrintStream, event: Any, n: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    for (c <- node.getChildren) 
    {
      println("printTypeCheckingWhileStatement: " + n + ", event: " + c.toString())
    }
    
    // "WHILE" "EXPRESSION" "DO" "STATEMENTSEQUENT"
    c =  node.getChildren.get(1)
    var exprReturn = printTypeCheckingExpression(stream, c, n)
    c =  node.getChildren.get(3)
    myVar = myVar + 1
    stream.println("\tn" + myVar +" [label=\"stmt list\",fillcolor=\"/x11/white\",shape=none]")
    stream.println("\tn1 -> n" + myVar)
    var stateSeqReturn = printTypeCheckingStatementSequence(stream, c, myVar)
    
    if (exprReturn != 2) return 1
    else return stateSeqReturn
  }
  
  private def printTypeCheckingIfStatement(stream: PrintStream, event: Any, n: Int): Int =
  {
    return 0
  }
  private def printTypeCheckingStatement(stream: PrintStream, event: Any, n: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    for (c <- node.getChildren) 
    {
      println("printTypeCheckingStatement: " + n + ", event: " + c.toString())
      if (c.toString() == "ASSIGNMENT") {
        return printTypeCheckingAssignment(stream, c, n)
      } else if (c.toString() == "writeInt") {
        return printTypeCheckingWriteInt(stream, c, n)
      } else if (c.toString() == "WHILESTATEMENT") {
        return printTypeCheckingWhileStatement(stream, c, n)
      }else if (c.toString() == "IFSTATEMENT") {
        return printTypeCheckingIfStatement(stream, c, n)
      }
      
    }
    return 0
  }
  
  private def printTypeCheckingStatementSequence(stream: PrintStream, event: Any, n: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    var returnType: Int = 1
    if (c != Node.E) 
    {
      // "STATEMENT" ";" "STATEMENTSEQUENCE"
      c = node.getChildren.get(0)
      var stateReturn = printTypeCheckingStatement(stream, c, n)
      
      c = node.getChildren.get(2)
      var stateSeqReturn = printTypeCheckingStatementSequence(stream, c, n)

      if (stateReturn == 1) return 1
      if (stateSeqReturn == 1) return 1
      return 0
    }
    return 0
  }
  
  private def printTypeChecking(stream: PrintStream, event: Any, n: Int): Int = 
  {
    var n1 = n
    var root = n1
    
    var node:Node = event.asInstanceOf[Node]
    var c: Any = node.getChildren.get(1)
    println("printTypeChecking: ",root + ", event: " + c.toString())
    stream.println("\tn2 [label=\"decl list\",fillcolor=\"/x11/white\",shape=none]")
    stream.println("\tn1 -> n2")
    n1 =  printTypeCheckingDeclarations(stream, c, n1+1)
    
    println("keys in symbolTable: " + symbolTable.keys)
    println("values in symbolTable: " + symbolTable.values)
    c = node.getChildren.get(3)
    println("printTypeChecking: ",root + ", event: " + c.toString())
    myVar = myVar + 1
    stream.println("\tn" + myVar +" [label=\"stmt list\",fillcolor=\"/x11/white\",shape=none]")
    stream.println("\tn1 -> n" + myVar)
    n1 = printTypeCheckingStatementSequence(stream, c, myVar)
    
    stream.println("\tn1" + " [label=\"program\",fillcolor=\"/pastel13/1" + "\",shape=box]")
    return n1
  }
    
}

class TypeCheckParser(fileName: String) {

  private var scanner: Scanner = new Scanner(fileName)

  def `match`(`type`: TokenType): Token = {
    val t = this.scanner.get
    if (t.getType != `type`) {
      throw new ParserError(`type`.toString, t)
    }
    return t
  }

  def parse(): Node = {
    this.scanner.scan()
    this.parseProgram()
  }

  // <program> ::= PROGRAM <declarations> BEGIN <statementSequence> END
  
  def parseProgram(): Node = {
    println("Inside Parse Program, after scan")
    val node = new Node(NodeType.program)
    println(node.toString())
    node.addToken(this.`match`(TokenType.program))
    node.addNode(this.parseDeclarations())
    node.addToken(this.`match`(TokenType.BEGIN))
    node.addNode(this.parseStatementSequence())
    node.addToken(this.`match`(TokenType.END))
    this.`match`(TokenType.EOF)
    println("Inside parseProgram before returns")

    return node
  }

  // <declarations> ::= VAR ident AS <type> SC <declarations> | E
  
  def parseDeclarations(): Node = {
    val node = new Node(NodeType.DECLARATIONS)
    if (this.scanner.look().getType == TokenType.VAR) 
    {
      println("Inside parseDeclarations, scanner: " + this.scanner.toString())
      node.addToken(this.`match`(TokenType.VAR))
      node.addToken(this.`match`(TokenType.IDENT))
      node.addToken(this.`match`(TokenType.AS))
      node.addNode(this.parseType())
      node.addToken(this.`match`(TokenType.SC))
      node.addNode(this.parseDeclarations())
    } 
    else 
    {
      node.addNode(Node.E);
    }
    return node
  }

  //<type> ::= INT | BOOL
  
  def parseType(): Node = {
    val node = new Node(NodeType.TYPE)
    if (this.scanner.lookType() == TokenType.BOOL) {
      node.addToken(this.`match`(TokenType.BOOL))
    } else if (this.scanner.lookType() == TokenType.INT) {
    	node.addToken(this.`match`(TokenType.INT))
    } else {
      throw new ParserError("type", this.scanner.look())
    }
    return node
  }

  // <statementSequence> ::= <statement> SC <statementSequence> | E
  
  def parseStatementSequence(): Node = {
    val node = new Node(NodeType.STATEMENTSEQUENCE)
    if (!this.scanner.isLookType(TokenType.END) && !this.scanner.isLookType(TokenType.ELSE) && 
      !this.scanner.isLookType(TokenType.EOF)) {
      node.addNode(this.parseStatement())
      node.addToken(this.`match`(TokenType.SC))
      node.addNode(this.parseStatementSequence())
    } else 
    {
      node.addNode(Node.E)
    }
    return node
  }

  // <statement> ::= <assignment>
        //  | <ifStatement>
        //  | <whileStatement>
        //  | <writeInt>
  
  def parseStatement(): Node = {
    val node = new Node(NodeType.STATEMENT)
    if (this.scanner.isLookType(TokenType.IF)) {
      node.addNode(this.parseIfStatement())
    } else if (this.scanner.isLookType(TokenType.WHILE)) {
      node.addNode(this.parseWhileStatement())
    } else if (this.scanner.isLookType(TokenType.WRITEINT)) {
      node.addNode(this.parseWriteInt())
    } else if (this.scanner.isLookType(TokenType.IDENT)) {
      node.addNode(this.parseAssignment())
    } else {
      throw new ParserError("Statement(IF|WHILE|READINT|IDENT)", this.scanner.look())
    }
    return node
  }
  
  // <ifStatement> ::= IF <expression> THEN <statementSequence> <elseClause> END

  def parseIfStatement(): Node = {
    val node = new Node(NodeType.IFSTATEMENT)
    node.addToken(this.`match`(TokenType.IF))
    node.addNode(this.parseExpression())
    node.addToken(this.`match`(TokenType.THEN))
    node.addNode(this.parseStatementSequence())
    node.addNode(this.parseElseClause())
    node.addToken(this.`match`(TokenType.END))
    return node
  }

  // <whileStatement> ::= WHILE <expression> DO <statementSequence> END
  
  def parseWhileStatement(): Node = {
    val node = new Node(NodeType.WHILESTATEMENT)
    node.addToken(this.`match`(TokenType.WHILE))
    node.addNode(this.parseExpression())
    node.addToken(this.`match`(TokenType.DO))
    node.addNode(this.parseStatementSequence())
    node.addToken(this.`match`(TokenType.END))
    return node
  }
  
  // <writeInt> ::= WRITEINT <expression>

  def parseWriteInt(): Node = {
    val node = new Node(NodeType.writeInt)
    node.addToken(this.`match`(TokenType.WRITEINT))
    node.addNode(this.parseExpression())
    return node
  }
  
  // <assignment> ::= ident ASGN <expression>
  //               |  ident ASGN READINT

  def parseAssignment(): Node = {
    val node = new Node(NodeType.ASSIGNMENT)
    node.addToken(this.`match`(TokenType.IDENT))
    node.addToken(this.`match`(TokenType.ASGN))
    if (this.scanner.isLookType(TokenType.READINT)) {
      node.addToken(this.`match`(TokenType.READINT))
    } else {
      node.addNode(this.parseExpression())
    }
    return node
  }

  // <elseClause> ::= ELSE <statementSequence> | E
  
  def parseElseClause(): Node = {
    val node = new Node(NodeType.ELSECLAUSE)
    if (this.scanner.isLookType(TokenType.ELSE)) {
      node.addToken(this.`match`(TokenType.ELSE))
      node.addNode(this.parseStatementSequence())
    } else {
      node.addNode(Node.E)
    }
    return node
  }

  // <expression> ::= <simpleExpression>
  //               | <simpleExpression> COMPARE <expression>
  
  def parseExpression(): Node = {
    val node = new Node(NodeType.EXPRESSION)
    node.addNode(this.parseSimpleExpression())
    if (this.scanner.isLookType(TokenType.COMPARE)) {
      node.addToken(this.`match`(TokenType.COMPARE))
      node.addNode(this.parseExpression())
    }
    return node
  }

  // <simpleExpression> ::= <term>
  //               | <term> ADDITIVE <simpleExpression>
  
  
  def parseSimpleExpression(): Node = {
    val node = new Node(NodeType.SIMPLEEXPRESSION)
    node.addNode(this.parseTerm())
    if (this.scanner.isLookType(TokenType.ADDITIVE)) {
      node.addToken(this.`match`(TokenType.ADDITIVE))
      node.addNode(this.parseSimpleExpression())
    }
    return node
  }
  
  // <term> ::= <factor> MULTIPLICATIVE <term>
  //          | <factor>

  def parseTerm(): Node = {
    val node = new Node(NodeType.TERM)
    node.addNode(this.parseFactor())
    if (this.scanner.isLookType(TokenType.MULTIPLICATIVE)) {
      node.addToken(this.`match`(TokenType.MULTIPLICATIVE))
      node.addNode(this.parseTerm())
    }
    return node
  }

 //<factor> ::= ident
 //         | num
 //         | boollit
 //         | LP <expression> RP
  
  def parseFactor(): Node = {
    val node = new Node(NodeType.FACTOR)
    if (this.scanner.isLookType(TokenType.LP)) {
      node.addToken(this.`match`(TokenType.LP))
      node.addNode(this.parseExpression())
      node.addToken(this.`match`(TokenType.RP))
    } else if (this.scanner.isLookType(TokenType.BOOLLIT)) {
      node.addToken(this.`match`(TokenType.BOOLLIT))
    } else if (this.scanner.isLookType(TokenType.IDENT)) {
      node.addToken(this.`match`(TokenType.IDENT))
    } else if (this.scanner.isLookType(TokenType.NUMBER)) {
      node.addToken(this.`match`(TokenType.NUMBER))
    } else {
      throw new ParserError("Factor(LP|IDENT|NUM|BOOLLIT)", this.scanner.look())
    }
    return node
  }
}
