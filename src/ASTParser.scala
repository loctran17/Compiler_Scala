import java.io._
import Node._;

import scala.collection.JavaConversions._
import TokenType._;

object ASTParser {
  var myVar: Int = 1
  var isProgram: Boolean = false 
  var isDeclList: Boolean = false
  var isStmtList: Boolean = false
  var varStmtList: Int = 0
  var prevVarStmtList: Int = 0
  var symbolTable: Map[String, Int] = Map()
  def toDotFile(fileName: String, node: Node) {
    val streamAST = new PrintStream(new FileOutputStream(fileName))
    streamAST.println("digraph tl12Ast {")
    streamAST.println("ordering=out;")
    streamAST.println("node [shape = box, style = filled];")
    printAST(streamAST, node, node, 1)
    streamAST.println("}")
  }
  
  private def printAST(stream: PrintStream, prevEvent: Any, event: Any, n: Int): Int = 
  {
    var n1 = n
    var root = n1
    
    if (event.isInstanceOf[Node]) 
    {
        println("printAST Instance of node, Inside printAST: ", root + ", event: " + event.toString() + ", prevEvent = " + prevEvent.toString())
        if (event.toString() == "program"  && isProgram == false){
          stream.println("\tn" + myVar + " [label=\"" + event.toString + "\", fillcolor = \"/x11/lightgrey\" ,shape = box]")
          isProgram = true
          myVar = myVar+1
        }
        
        else if   (event.toString() == "DECLARATIONS" && isDeclList == false) {
          stream.println("\tn" + myVar + " [label=\"decl list\"" +  ", fillcolor = \"/x11/white\" ,shape = none]")
          stream.println("\tn1 -> " + "n" + myVar)
          myVar = myVar+1
          isDeclList = true
        }
        else if   (event.toString() == "STATEMENTSEQUENCE") {
          var node:Node = event.asInstanceOf[Node]
          if (prevEvent.toString() ==  "DO" || prevEvent.toString() ==  "THEN" || 
              prevEvent.toString() ==  "ELSE" || prevEvent.toString() ==  "program")
          {
            if (node.getChildren.size() > 0) {
              stream.println("\tn" + myVar + " [label=\"stmt list\"" +  ", fillcolor = \"/x11/white\" ,shape = none]")
              stream.println("\tn" + root  + " -> " + "n" + myVar)
              myVar = myVar + 1
              if (varStmtList != 0) prevVarStmtList = varStmtList
              varStmtList = myVar
              
            }
          }
          
          if (node.getChildren.size() > 1) {
            var tmpVal: Int = varStmtList.toInt
            
            // 
            // Statement
            var node:Node = event.asInstanceOf[Node]
            var tmp: Any= node.getChildren.get(0)
            printAST(stream, event, tmp, tmpVal-1)
            
            tmp = node.getChildren.get(1)
            printAST(stream, event, tmp, tmpVal-1)
            if (prevVarStmtList != 0) varStmtList = prevVarStmtList  
            }
        }
       
        if (event.toString() == "ASSIGNMENT") {
          var assignString: String = ":="
          var count: Int = 0
          var node:Node = event.asInstanceOf[Node]
          var tmp: Any= node.getChildren.get(2) 
          println("get(2) in ASSIGNEMTN toString = " + tmp.toString())
          if (tmp.toString() == "READINT") {
              assignString = assignString.concat(" ")
              assignString = assignString.concat(tmp.toString())
              println("AssignString = " + assignString)
          }
          stream.println("\tn" + myVar + " [label=\"" + assignString 
                    + "\",fillcolor=\"/x11/lightgrey\",shape=box]")
          stream.println("\tn" + n + " -> " + "n" + myVar)
          myVar = myVar + 1
              
          tmp = node.getChildren.get(0)
          println("get(0) in ASSIGNEMTN toString = " + tmp.toString())
          if (tmp.toString().length() > 7 && tmp.toString().substring(0, 7) == "IDENT :") {
            stream.println("\tn" + myVar + " [label=\"" + tmp.toString().substring(8, tmp.toString().length())
                    + "\",fillcolor=\"/pastel13/3\",shape=box]")
            stream.println("\tn" +( myVar -1) + " -> " + "n" + myVar)
            myVar = myVar + 1
          }
          
          // Expression
          tmp = node.getChildren.get(2)
          if (tmp.toString() != "READINT") {
              printAST(stream, prevEvent, tmp, myVar - 2)
          }
          return n1
        }
        
        if (event.toString() == "WHILESTATEMENT") {
          var assignString: String = ":="
          var count: Int = 0
          var node:Node = event.asInstanceOf[Node]
          var tmpVal: Int = myVar
          var tmp: Any= node.getChildren.get(0) 
          
          // While
          stream.println("\tn" + myVar + " [label=\"" + tmp.toString() 
                    + "\",fillcolor=\"/x11/lightgrey\",shape=box]")
          stream.println("\tn" + n + " -> " + "n" + myVar)
          myVar = myVar + 1
          
          // Expression
          tmp = node.getChildren.get(1) 
          n1 = printAST(stream, event, tmp, tmpVal)
          
          // DO
          tmp = node.getChildren.get(2) 
//          stream.println("\tn" + myVar + " [label=\"" + tmp.toString() 
//                    + "\",fillcolor=\"/x11/lightgrey\",shape=box]")
//          stream.println("\tn" + varStmtList + " -> " + "n" + myVar)
//          myVar = myVar + 1
          
          tmp = node.getChildren.get(3) 
          n1 = printAST(stream, node.getChildren.get(2), tmp, tmpVal)
          
          // END
          tmp = node.getChildren.get(4) 
//          stream.println("\tn" + myVar + " [label=\"" + tmp.toString() 
//                    + "\",fillcolor=\"/x11/lightgrey\",shape=box]")
//          stream.println("\tn" + varStmtList + " -> " + "n" + myVar)
//          myVar = myVar + 1
          return n1
        }
        
        if (event.toString() == "writeInt") {
          var node:Node = event.asInstanceOf[Node]
          var tmpVal:Int = myVar
          var tmp: Any= node.getChildren.get(0) 
          if (tmp.toString() == "WRITEINT") {
            stream.println("\tn" + myVar + " [label=\"" + tmp.toString() 
                    + "\",fillcolor=\"/x11/lightgrey\",shape=box]")
            stream.println("\tn" + n + " -> " + "n" + myVar)
            myVar = myVar + 1
          }
          tmp = node.getChildren.get(1)
          println("get(1) in writeInt toString = " + tmp.toString())
          if (tmp.toString() == "EXPRESSION") {
              n1 = printAST(stream, event, tmp, tmpVal)
          }
          return n1
        }
        
        if (event.toString() == "EXPRESSION") {
          var node:Node = event.asInstanceOf[Node]
          var tmp: Any= node.getChildren.get(0) 
          var tmpVal:Int = myVar
          println("get(0) in EXPRESSION toString = " + tmp.toString())
          
          if (node.getChildren.size() > 1) {
            tmp = node.getChildren.get(1) 
            stream.println("\tn" + myVar + " [label=\"" + tmp.toString() 
                    + "\",fillcolor=\"/pastel13/3\",shape=box]")
            stream.println("\tn" + n + " -> " + "n" + myVar)
            myVar = myVar + 1
            tmp = node.getChildren.get(0)
            printAST(stream, event, tmp, tmpVal)
            tmp = node.getChildren.get(2)
            return printAST(stream, event, tmp, tmpVal)
          } else {
            tmp = node.getChildren.get(0)
            return printAST(stream, event, tmp, root)
          }
        }
        
        if (event.toString() == "SIMPLEEXPRESSION") {
          var node:Node = event.asInstanceOf[Node]
          var tmp: Any= node.getChildren.get(0) 
          var tmpVal:Int = myVar
          println("get(0) in SIMPLEEXPRESSION toString = " + tmp.toString())
          
          if (node.getChildren.size() > 1) {
            tmp = node.getChildren.get(1) 
            stream.println("\tn" + myVar + " [label=\"" + tmp.toString() 
                    + "\",fillcolor=\"/pastel13/3\",shape=box]")
            stream.println("\tn" + n + " -> " + "n" + myVar)
            myVar = myVar + 1
            tmp = node.getChildren.get(0)
            printAST(stream, event, tmp, tmpVal)
            tmp = node.getChildren.get(2)
            return printAST(stream, event, tmp, tmpVal)
          } else {
            tmp = node.getChildren.get(0)
            return printAST(stream, event, tmp, root)
          }
        }
        
        
        if (event.toString() == "TERM") {
          var node:Node = event.asInstanceOf[Node]
          var tmp: Any= node.getChildren.get(0) 
          var tmpVal:Int = myVar
          println("get(0) in TERM toString = " + tmp.toString())
          
          if (node.getChildren.size() > 1) {
            tmp = node.getChildren.get(1) 
            
            stream.println("\tn" + myVar + " [label=\"" + tmp.toString() 
                    + "\",fillcolor=\"/pastel13/3\",shape=box]")
            stream.println("\tn" + n + " -> " + "n" + myVar)
            myVar = myVar + 1
            tmp = node.getChildren.get(0)
            printAST(stream, event, tmp, tmpVal)
            tmp = node.getChildren.get(2)
            return printAST(stream, event, tmp, tmpVal)
          } else {
            tmp = node.getChildren.get(0)
            return printAST(stream, event, tmp, root)
          }
        }
        
        if (event.toString() == "FACTOR") {
          var node:Node = event.asInstanceOf[Node]
          var tmp: Any= node.getChildren.get(0) 
          var tmpVal:Int = myVar
          println("get(0) in FACTOR toString = " + tmp.toString())
          
          if (node.getChildren.size() > 1) {
            tmp = node.getChildren.get(1) 
            stream.println("\tn" + myVar + " [label=\"" + tmp.toString() 
                    + "\",fillcolor=\"/pastel13/3\",shape=box]")
            stream.println("\tn" + n + " -> " + "n" + myVar)
            myVar = myVar + 1
            tmp = node.getChildren.get(0)
            printAST(stream, event, tmp, tmpVal)
            tmp = node.getChildren.get(2)
            return printAST(stream, event, tmp, tmpVal)
          } else {
            tmp = node.getChildren.get(0)
            println("get(0) in FACTOR toString = " + tmp.toString() + "tmpVal = " + tmpVal)
            return printAST(stream, event, tmp, n)
          }
        }
        
    } 
    else 
    {
        println("printAST Not instance of node, Inside printParseTree: ",root + ", event: " + event.toString())
        println("printAST Not instance of node, Inside printParseTree: ", root + ", event: " + event.toString() + ", prevEvent = " + prevEvent.toString())
         
        // Decl varaible
        if (event.toString().length() > 7 && event.toString().substring(0, 7) == "IDENT :" && prevEvent.toString() == "DECLARATIONS") {
          println("\tn" + myVar + " [label=\"decl: \'" + 
              event.toString().substring(8, event.toString().length()) + "\'\"]")
          stream.println("\tn" + myVar + " [label=\"decl: \'" + 
              event.toString().substring(8, event.toString().length()) + "\'\", fillcolor=\"/pastel13/3\",shape=box]")
          stream.println("\tn2" + " -> " + "n" + myVar)    
          myVar = myVar+1
          
          
        }
        
        if (event.toString().length() > 8 && event.toString().substring(0, 8) == "NUMBER :" && prevEvent.toString() == "FACTOR") {
          stream.println("\tn" + myVar + " [label=\"" + 
              event.toString().substring(9, event.toString().length()) + "\",fillcolor=\"/pastel13/3\",shape=box]")
          stream.println("\tn" + n + " -> " + "n" + myVar)    
          println("\tn" + n + " -> " + "n" + myVar)
          myVar = myVar+1
          
          
        }
        
        // Decl varaible
        if (event.toString().length() > 7 && event.toString().substring(0, 7) == "IDENT :" && prevEvent.toString() == "FACTOR") {
          stream.println("\tn" + myVar + " [label=\"" + 
              event.toString().substring(8, event.toString().length()) + "\",fillcolor=\"/pastel13/3\",shape=box]")
          stream.println("\tn" + n + " -> " + "n" + myVar)    
          println("\tn" + n + " -> " + "n" + myVar)
          myVar = myVar+1
        }
        
        // Type 
        if (prevEvent.toString() == "TYPE") {
          stream.println("\tn" + myVar + " [label=\"" + event.toString().toLowerCase() 
              + "\",fillcolor=\"/x11/lightgrey\",shape=box]")
          stream.println("\tn" +( myVar -1) + " -> " + "n" + myVar)    
          myVar = myVar+1
        }
        
        // 
        //stream.println("\tn" + root + " [label=\"" + event.toString + "\"]")
      //return scala.N;
    }
    if (event.isInstanceOf[Node]) 
    {
      println("printAST Inside printAST: ",root)
  
      var node:Node = event.asInstanceOf[Node]
      for (c <- node.getChildren) 
      {
 
        //var current: Int = {n1 + 1}
        //println("printAST Inside printParseTree, current: "+ current)
        if (event.isInstanceOf[Node] && c.toString() != "STATEMENT") {
          n1 =  printAST(stream, event, c, n1)
        }
        //stream.println("\tn" + root + " -> " + "n" + current)
      }
    }
   return n1
  }
  
}

class ASTParser(fileName: String) {

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
      node.addNode(this.parseSimpleExpression())
    }
    return node
  }

  // <simpleExpression> ::= <term>
  //               | <term> ADDITIVE <term>
  
  
  def parseSimpleExpression(): Node = {
    val node = new Node(NodeType.SIMPLEEXPRESSION)
    node.addNode(this.parseTerm())
    if (this.scanner.isLookType(TokenType.ADDITIVE)) {
      node.addToken(this.`match`(TokenType.ADDITIVE))
      node.addNode(this.parseTerm())
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
      node.addNode(this.parseFactor())
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
