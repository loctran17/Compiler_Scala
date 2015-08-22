import java.io._
import Node._;

import scala.collection.JavaConversions._
import TokenType._;

object CfgGen {
  var nodeCurrent: Int = 0
  var nodeStep: Int = 0
  var exitNode: Int = 0
  var isProgram: Boolean = false 
  var isDeclList: Boolean = false
  var isStmtList: Boolean = false
  var varStmtList: Int = 0
  var prevVarStmtList: Int = 0
  var registerIndex: Int = 0
  var symbolTable: Map[String, Int] = Map()
  var nodeTable: Map[String, Int] = Map()
  def toDotFile(fileNameAST: String, node: Node): Int = {
    val streamCfgGen = new PrintStream(new FileOutputStream(fileNameAST))
    streamCfgGen.println("digraph graphviz {")
    streamCfgGen.println("\tnode [shape = none];")
    streamCfgGen.println("\tedge [tailport = s];")
    streamCfgGen.println("\tentry")
    streamCfgGen.println("\tsubgraph cluster {")
    streamCfgGen.println("\tcolor=\"/x11/white\"")
    
    var returnType = printCfg(streamCfgGen, node, 0)
    streamCfgGen.println("}")
    return returnType
  }
  
  // Build Symbol Table here 
  private def DeclarationsGen(stream: PrintStream, event: Any): Int =
  {
    
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    
    if (c != Node.E) {
      // "VAR" "INDENT : X" "AS" "TYPE" ";"
      c = node.getChildren.get(1)
      
      // Varible name
      var varible: String = c.toString().substring(8, c.toString().length())
      
      // Type 
      c =  node.getChildren.get(3)
      var nodeType:Node = c.asInstanceOf[Node]
      var declType: Int = 0
      var cc:Any = nodeType.getChildren.get(0)
      if (cc.toString() == "INT") declType = 3
      else if (cc.toString() == "BOOL") declType = 2
      else declType = 1
      
      symbolTable += (varible -> declType )
      
      // Declaration
      c = node.getChildren.get(5)
      DeclarationsGen(stream, c)
    }
    
    
    return 0
  }
  
  
  private def printCfgAssignment(stream: PrintStream, event: Any, n: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
        
    // INDENT : X" ASIGN "READINT/EXPRESSION"
    c = node.getChildren.get(0)
    var varible: String = c.toString().substring(8, c.toString().length())
    
    var declType = symbolTable.get(varible)
    var returnType: Int = 0
    
    c= node.getChildren.get(2)
    if (c.toString() == "READINT") {
      stream.println("\t\t<tr>")
      stream.println("\t\t<td align=\"left\">readInt</td>")
      stream.println("\t\t<td align=\"left\"></td>")
      stream.println("\t\t<td align=\"left\">=&gt; r_" + varible + "</td>")
      stream.println("\t\t</tr>")
      
      
      } 
    else {
      var returnExpr = printCfgExpression(stream, c, n)
      stream.println("\t\t<tr>")
      stream.println("\t\t<td align=\"left\">i2i</td>")
      stream.println("\t\t<td align=\"left\">" + returnExpr + "</td>")
      stream.println("\t\t<td align=\"left\">=&gt; r_" + varible + "</td>")
      stream.println("\t\t</tr>")
        
      
    }
    return  nodeCurrent
    
  }
  
  
  // Factor can return Bool, Int, Ident, or Expr
  private def printCfgFactor(stream: PrintStream, event: Any, n: Int): String =
  {
    if (event.isInstanceOf[Node]) {
      var node:Node = event.asInstanceOf[Node]
      var c:Any = node.getChildren.get(0)
      
      if (node.getChildren.size() == 1) {
         if (c.toString().length() > 8 && c.toString().substring(0, 8) == "NUMBER :" ) {
            stream.println("\t\t<tr>")
            stream.println("\t\t<td align=\"left\">loadI</td>")
            stream.println("\t\t<td align=\"left\">" + c.toString().substring(9, c.toString().length()) +
                          "</td>")
            stream.println("\t\t<td align=\"left\">=&gt; r" + registerIndex + "</td>")
            stream.println("\t\t</tr>")
            registerIndex = registerIndex + 1
           
            return "r" + (registerIndex-1)
         }
         if (c.toString().length() > 7 && c.toString().substring(0, 7) == "IDENT :") {
              var variable: String = c.toString().substring(8, c.toString().length())
              var declType = symbolTable.get(variable) 
              return "r_" + variable
         }
         if (c.toString().length() > 6 && c.toString().substring(0, 6) == "BOOL :") {
              
              return "BOOL"
         }
      } else {
          c = node.getChildren.get(1)
          printCfgExpression(stream, event, n)
      }
      
    }
    else {
      if (event.toString().length() > 8 && event.toString().substring(0, 8) == "NUMBER :" ) {
            stream.println("\t\t<tr>")
            stream.println("\t\t<td align=\"left\">loadI</td>")
            stream.println("\t\t<td align=\"left\">" + event.toString().substring(9, event.toString().length()) +
                          "</td>")
            stream.println("\t\t<td align=\"left\">=&gt; r" + registerIndex + "</td>")
            stream.println("\t\t</tr>")
            registerIndex = registerIndex + 1
           
            return "r" + (registerIndex-1)
         }
         if (event.toString().length() > 7 && event.toString().substring(0, 7) == "IDENT :") {
              var variable: String = event.toString().substring(8, event.toString().length())
              var declType = symbolTable.get(variable) 
              return variable
         }
         if (event.toString().length() > 6 && event.toString().substring(0, 6) == "BOOL :") {
              
              return "BOOL"
         }
    }  
    
    return "0INVL"
  }
  
  private def printCfgTerm(stream: PrintStream, event: Any, n: Int): String =
  {
   if (event.isInstanceOf[Node])  {
      var node:Node = event.asInstanceOf[Node]
      var c:Any = node.getChildren.get(0)
      var returnType: Int = 0
      
      // factor
      if (node.getChildren.size() == 1) {
        return printCfgFactor(stream, c, n)
      } else {
        //myVar = myVar+1
        c = node.getChildren.get(0)
        var factor1  = printCfgFactor(stream, c, n)
        c = node.getChildren.get(2)
        var factor2 = printCfgFactor(stream, c, n)
        c = node.getChildren.get(1)
        if (c.toString() == "*") {
          stream.println("\t\t<tr>")
          stream.println("\t\t<td align=\"left\">mult</td>")
          stream.println("\t\t<td align=\"left\">" + factor1 + "," + factor2 + "</td>")
          stream.println("\t\t<td align=\"left\">=&gt; r" + registerIndex + "</td>")
          stream.println("\t\t</tr>")
          registerIndex = registerIndex + 1
        } else if (c.toString() == "/") {
          stream.println("\t\t<tr>")
          stream.println("\t\t<td align=\"left\">div</td>")
          stream.println("\t\t<td align=\"left\">" + factor1 + "," + factor2 + "</td>")
          stream.println("\t\t<td align=\"left\">=&gt; r" + registerIndex + "</td>")
          stream.println("\t\t</tr>")
          registerIndex = registerIndex + 1
        }
        return "r" + (registerIndex-1)
      } 
    } else {
      return printCfgFactor(stream, event, n)
    }
   return "0INVL"
  }
  
  private def printCfgSimpleExpression(stream: PrintStream, event: Any, n: Int): String =
  {
    if (event.isInstanceOf[Node]) {
      var node:Node = event.asInstanceOf[Node]
      var c:Any = node.getChildren.get(0)
      
      
      // TERM
      if (node.getChildren.size() == 1) {
        return printCfgTerm(stream, c, n)
      } else {
        c = node.getChildren.get(0)
        var term = printCfgTerm(stream, c, n)
        c = node.getChildren.get(2)
        var expr = printCfgSimpleExpression(stream, c, n)
        c = node.getChildren.get(1)
        if (c.toString() == "+") {
          stream.println("\t\t<tr>")
          stream.println("\t\t<td align=\"left\">add</td>")
          stream.println("\t\t<td align=\"left\">" + term + "," + expr + "</td>")
          stream.println("\t\t<td align=\"left\">=&gt; r" + registerIndex + "</td>")
          stream.println("\t\t</tr>")
          
        
        } else  if (c.toString() == "-") {
          stream.println("\t\t<tr>")
          stream.println("\t\t<td align=\"left\">sub</td>")
          stream.println("\t\t<td align=\"left\">" + term + "," + expr + "</td>")
          stream.println("\t\t<td align=\"left\">=&gt; r" + registerIndex + "</td>")
          stream.println("\t\t</tr>")
          
        }
        registerIndex = registerIndex + 1
        return "r" + (registerIndex - 1) 
      } 
    } else {
      return printCfgTerm(stream, event, n)
    } 
      
    return "0INVL"
  }
  
  private def printCfgExpression(stream: PrintStream, event: Any, n: Int): String =
  {
    var node:Node = event.asInstanceOf[Node]
    
    var c:Any = node.getChildren.get(0)
    
    // <simpleExpression> case 
    if (node.getChildren.size() == 1) {
      c = node.getChildren.get(0)
      return printCfgSimpleExpression(stream, c, n)
    } else {
      // Add Compare to the tree, must be boolean
      c = node.getChildren.get(0)
      var simpleReturn = printCfgSimpleExpression(stream, c, n)
      c = node.getChildren.get(2)
      var expr = printCfgExpression(stream, c, n)
      
      
      c = node.getChildren.get(1)
      if (c.toString() == "<") {
        stream.println("\t\t<tr>")
        stream.println("\t\t<td align=\"left\">cmp_LT</td>")
        stream.println("\t\t<td align=\"left\">" + simpleReturn + "," + expr + "</td>")
        stream.println("\t\t<td align=\"left\">=&gt; r" + registerIndex + "</td>")
        stream.println("\t\t</tr>")
        
      } else if (c.toString() == "<=") {
        stream.println("\t\t<tr>")
        stream.println("\t\t<td align=\"left\">cmp_LTE</td>")
        stream.println("\t\t<td align=\"left\">" + simpleReturn + "," + expr + "</td>")
        stream.println("\t\t<td align=\"left\">=&gt; r" + registerIndex + "</td>")
        stream.println("\t\t</tr>")
         
      } else if (c.toString() == ">") {
        stream.println("\t\t<tr>")
        stream.println("\t\t<td align=\"left\">cmp_GT</td>")
        stream.println("\t\t<td align=\"left\">" + simpleReturn + "," + expr + "</td>")
        stream.println("\t\t<td align=\"left\">=&gt; r" + registerIndex + "</td>")
        stream.println("\t\t</tr>")
         
      } else if (c.toString() == ">=") {
        stream.println("\t\t<tr>")
        stream.println("\t\t<td align=\"left\">cmp_GTE</td>")
        stream.println("\t\t<td align=\"left\">" + simpleReturn + "," + expr + "</td>")
        stream.println("\t\t<td align=\"left\">=&gt; r" + registerIndex + "</td>")
        stream.println("\t\t</tr>")
         
      } else if (c.toString() == "=") {
        stream.println("\t\t<tr>")
        stream.println("\t\t<td align=\"left\">cmp_EQ</td>")
        stream.println("\t\t<td align=\"left\">" + simpleReturn + "," + expr + "</td>")
        stream.println("\t\t<td align=\"left\">=&gt; r" + registerIndex + "</td>")
        stream.println("\t\t</tr>")
      }
      
      registerIndex = registerIndex + 1 
      return "r" + (registerIndex-1)
    }
    
  }
  
  private def printCfgWriteInt(stream: PrintStream, event: Any, n: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    
    c = node.getChildren.get(1)
    var exprReturn = printCfgExpression(stream, c, n)
    
    // 
    stream.println("\t\t<tr>")
    stream.println("\t\t<td align=\"left\">writeInt</td>")
    stream.println("\t\t<td align=\"left\"> " + exprReturn + "</td>")
    stream.println("\t\t<td align=\"left\"></td>")
    stream.println("\t\t</tr>")
        
    return nodeCurrent
  }
  
  private def printCfgWhileStatement(stream: PrintStream, event: Any, n: Int, parentNode: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
        
    if (n == 0) {
      exitNode = nodeCurrent
      println(" exitNode = " + exitNode)
    }
    // "WHILE" "EXPRESSION" "DO" "STATEMENTSEQUENT"
    c =  node.getChildren.get(1)
    var exprReturn = printCfgExpression(stream, c, n)
    
    
    var tempNode: Int = nodeCurrent
    stream.println("\t\t<tr>")
    stream.println("\t\t<td align=\"left\">cbr</td>")
    stream.println("\t\t<td align=\"left\">" + exprReturn + "</td>")
    stream.println("\t\t<td align=\"left\">-&gt; B" + (nodeCurrent+1) + ", B" + (nodeCurrent + 2)+ "</td>")
    stream.println("\t\t</tr></table>>,fillcolor=\"/x11/white\",shape=box]")
    stream.println("\tn" + (nodeCurrent-1) + "-> n" + nodeCurrent)
    stream.println("\tn" + (nodeCurrent-1) + "-> n" + (nodeCurrent+1))
    
    stream.println("\tn" + (nodeCurrent+1) + " [label=<<table border=\"0\">")
    stream.println("\t\t<tr>")
    stream.println("\t\t<td border=\"1\" colspan=\"3\">B" + (nodeCurrent+1) + "</td>")
    stream.println("\t\t</tr>")
    var nodeString: String = "B"+(nodeCurrent+1)
    nodeTable += (nodeString -> (nodeCurrent+1))
    
    nodeCurrent = nodeCurrent + 2
    nodeStep = 2
    c =  node.getChildren.get(3)
    var stateSeqReturn = printCfgStatementSequence(stream, c, n+1)
    stream.println("\t\t<tr>")
    stream.println("\t\t<td align=\"left\">jumpI</td>")
    stream.println("\t\t<td align=\"left\"></td>")
    stream.println("\t\t<td align=\"left\">-&gt; B" + tempNode + "</td>")
    stream.println("\t\t</tr>")
    stream.println("\t\t</table>>,fillcolor=\"/x11/white\",shape=box]")
    nodeString = "B"+tempNode
    if (tempNode == (nodeCurrent - 2)) stream.println("\tn" + (tempNode + 1) + "-> n" + (tempNode-1))
    else if (tempNode == 2) stream.println("\tn" + (nodeCurrent - 2) + "-> n" + (tempNode-1))
    
    //stream.println("\tn" + tempNode + "-> n" + nodeTable.get(nodeString).get)
    
    nodeString = "B"+(tempNode+2)
    nodeTable += (nodeString -> tempNode)
    
    println("JUMPI2 currentNode = " + nodeCurrent + ", B" + tempNode)
    println("JUMPI2 tempNode = " + tempNode + ", B" + tempNode)
    
    stream.println("\tn" + (tempNode) + " [label=<<table border=\"0\">")
    stream.println("\t\t<tr>")
    stream.println("\t\t<td border=\"1\" colspan=\"3\">B" + (tempNode+2) + "</td>")
    stream.println("\t\t</tr>")
    
          
    
    return (tempNode+2)
  }
  
  private def printCfgIfStatement(stream: PrintStream, event: Any, n: Int): Int =
  {
    return 0
  }
  private def printCfgStatement(stream: PrintStream, event: Any, n: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    for (c <- node.getChildren) 
    {
      println("printCfgStatement: " + n + ", event: " + c.toString())
      if (c.toString() == "ASSIGNMENT") {
        return printCfgAssignment(stream, c, n)
      } else if (c.toString() == "writeInt") {
        return printCfgWriteInt(stream, c, n)
      } else if (c.toString() == "WHILESTATEMENT") {
        var tempNode: Int = nodeCurrent
        stream.println("\t\t<tr>")
        stream.println("\t\t<td align=\"left\">jumpI</td>")
        stream.println("\t\t<td align=\"left\"></td>")
        stream.println("\t\t<td align=\"left\">-&gt; B" + (nodeCurrent+1) + "</td>")
        stream.println("\t\t</tr>")
        println("JUMPI1 currentNode = " + nodeCurrent + ", B" + (nodeCurrent+1) )
        stream.println("\t\t</table>>,fillcolor=\"/x11/white\",shape=box]")
        stream.println("\tn" + (nodeCurrent-1) + " -> n" + nodeCurrent)
        stream.println("\tn" + nodeCurrent + " [label=<<table border=\"0\">")
        stream.println("\t\t<tr>")
        stream.println("\t\t<td border=\"1\" colspan=\"3\">B" + (nodeCurrent + 1) + "</td>")
        stream.println("\t\t</tr>")
        
        var nodeString: String  = "B"+(nodeCurrent + 1)
        nodeTable += (nodeString -> nodeCurrent)
    
        nodeCurrent = nodeCurrent + 1
        nodeStep = 1
        
        var returnVal = printCfgWhileStatement(stream, c, n, nodeCurrent)
        println("while return = " + returnVal)
        return returnVal
        
      } else if (c.toString() == "IFSTATEMENT") {
        return printCfgIfStatement(stream, c, n)
      }
      
    }
    return 0
  }
  
  private def printCfgStatementSequence(stream: PrintStream, event: Any, n: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    var returnType: Int = 1
    if (c != Node.E) 
    {
      // "STATEMENT" ";" "STATEMENTSEQUENCE"
      c = node.getChildren.get(0)
      var stateRet = printCfgStatement(stream, c, n)
      
      c = node.getChildren.get(2)
      if (c != Node.E) {
        var seqState  = printCfgStatementSequence(stream, c, n) 
        return   seqState
      }
      else {
        return stateRet
      }  
      
    } else {
      return nodeCurrent
    }
    
  }
  
  private def printCfg(stream: PrintStream, event: Any, n: Int): Int = 
  {
    var n1 = n
    var root = n1
    
    var node:Node = event.asInstanceOf[Node]
    var c: Any = node.getChildren.get(1)
    println("printCfg: ",root + ", event: " + c.toString())
    n1 =  DeclarationsGen(stream, c)
    
    c = node.getChildren.get(3)
    // First node in cluster (n0 node)
    stream.println("\tn0 [label=<<table border=\"0\">")
    stream.println("\t\t<tr>")
    stream.println("\t\t<td border=\"1\" colspan=\"3\">B" + (nodeCurrent+1) + "</td>")
    stream.println("\t\t</tr>")
    nodeCurrent = nodeCurrent+1
    nodeStep = 1 
    
    for ((k,v)<- symbolTable) {
      printf("key: %s, value: %s\n", k, v)
      stream.println("\t\t<tr>")
      stream.println("\t\t<td align=\"left\">loadI</td>")
      stream.println("\t\t<td align=\"left\">0</td>")
      stream.println("\t\t<td align=\"left\">=&gt; r_" +k + "</td>")
      stream.println("\t\t</tr>")
      
    }
    println("printCfg: ",root + ", event: " + c.toString())
    n1 = printCfgStatementSequence(stream, c, root)
    println("currentNode = " + nodeCurrent + "returnStatementSeq = " + n1)
        
    
    // End of current node
    stream.println("\t\t<tr>")
    stream.println("\t\t<td align=\"left\">exit</td>")
    stream.println("\t\t<td align=\"left\"></td>")
    stream.println("\t\t<td align=\"left\"></td>")
    stream.println("\t\t</tr>")
    stream.println("\t</table>>,fillcolor=\"/x11/white\",shape=box]")
    
    // End of subgraph cluster
    println("exit: currentNode = " + (nodeCurrent-1))
    println("exit: exitNode = " + exitNode)
    stream.println("\t}")
    stream.println("\tentry -> n0")
    if (exitNode != 0 ) 
      stream.println("\tn" + exitNode + " -> exit")
    else 
      stream.println("\tn" + (nodeCurrent-1) + " -> exit")
    return n1
  }
    
}

class CfgGen(fileName: String) {

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
