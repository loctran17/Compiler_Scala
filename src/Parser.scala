import java.io._
import Node._;

import scala.collection.JavaConversions._
import TokenType._;

object Parser {

  def toDotFile(fileName: String, node: Node) {
    val stream = new PrintStream(new FileOutputStream(fileName))
    stream.println("digraph parseTree {")
    stream.println("ordering=out;")
    stream.println("node [shape = box, style = filled];")
    printParseTree(stream, node, 0)
    stream.println("}")
  }

  private def printParseTree(stream: PrintStream, event: Any, n: Int): Int = 
  
  {
    
    var n1 = n
    var root = n1
    if (event == Node.E) 
    {
      //println("Inside printParseTree: ",root)
      stream.println("\tn" + root + " [label=\"&#949;\", shape=none]")
      return n1
    }
    if (event.isInstanceOf[Node]) 
    {
        //println(" Instance of node, Inside printParseTree: ",root + ", event: " + event.toString())
        stream.println("\tn" + root + " [label=\"" + event.toString + "\", fillcolor = \"/x11/white\" ,shape = box]")
    } 
    else 
    {
        //println("Not instance of node, Inside printParseTree: ",root + ", event: " + event.toString())
        stream.println("\tn" + root + " [label=\"" + event.toString + "\"]")
    }
    if (event.isInstanceOf[Node]) 
    {
          //println("Inside printParseTree: ",root)
  
      var node:Node = event.asInstanceOf[Node]
      for (c <- node.getChildren) 
      {
 
        var current: Int = {n1+ 1}
        //println("Inside printParseTree, current: "+ current)
        n1 =  printParseTree(stream, c,current)
        stream.println("\tn" + root + " -> " + "n" + current)
      }
    }
   return n1
  }
}

class Parser(fileName: String) {

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
