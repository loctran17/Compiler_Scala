import java.io._
import Node._;

import scala.collection.JavaConversions._
import TokenType._;

object AsmGen {
  var nodeCurrent: Int = 0
  var nodeStep: Int = 0
  var exitNode: Int = 0
  var isProgram: Boolean = false 
  var isDeclList: Boolean = false
  var isStmtList: Boolean = false
  var varStmtList: Int = 0
  var prevVarStmtList: Int = 0
  var registerIndex: Int = 0
  var numberVariable: Int = 0
  var symbolTable: Map[String, Int] = Map()
  var symbolIndexTable: Map[String, Int] = Map()
  var symbolArray: Array[String] = new Array[String](100)
  var nodeTable: Map[String, Int] = Map()
  var lableL: Int = 11  
  def toDotFile(fileNameAsm: String, node: Node) {
    val streamAsmGen = new PrintStream(new FileOutputStream(fileNameAsm))
    streamAsmGen.println("\t.data")
    streamAsmGen.println("newline:  .asciiz \"\\n\"")
    streamAsmGen.println("\t.text")
    streamAsmGen.println("\t.globl main")
    streamAsmGen.println("main:")
    streamAsmGen.println("\tli $fp, 0x7ffffffc")
    streamAsmGen.println()
    printAsm(streamAsmGen, node, 0)
  
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
      symbolIndexTable += (varible -> numberVariable )
      symbolArray(numberVariable) = varible
      numberVariable = numberVariable + 1
      
      // Declaration
      c = node.getChildren.get(5)
      DeclarationsGen(stream, c)
    }
    
    
    return 0
  }
  
  
  private def printAsmAssignment(stream: PrintStream, event: Any, n: Int): Int =
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
      stream.println("\t# readInt  => r_" + varible)
      stream.println("\tli $v0, 5")
      stream.println("\tsyscall")
      stream.println("\tadd $t0, $v0, $zero")
      var index = symbolIndexTable.get(varible)
      if (index.get * 4 > 0)
        stream.println("\tsw $t0, -" + index.get*4 + "($fp)")
      else 
        stream.println("\tsw $t0, 0($fp)")
      stream.println()  
    } 
    else {
      var returnExpr = printAsmExpression(stream, c, n)
      
      var size = symbolIndexTable.size
      stream.print("\t# i2i ")
      if (returnExpr < size) 
        stream.print("r_" + symbolArray(returnExpr))
      else       
      {
        stream.print("r" + (returnExpr - size) )
      }
      stream.println(" => r_" + varible)
      if (returnExpr > 0)
        stream.println("\tlw $t1, -" + 4*returnExpr + "($fp)")
      else   
        stream.println("\tlw $t1, 0($fp)")
      stream.println("\tadd $t0, $t1, $zero")
      var index = symbolIndexTable.get(varible)
      if (index.get > 0)
        stream.println("\tsw $t0, -" + (index.get*4) + "($fp)")
      else 
        stream.println("\tsw $t0, 0($fp)")
      stream.println()  
    }
    return  nodeCurrent
    
  }
  
  
  // Factor can return Bool, Int, Ident, or Expr
  private def printAsmFactor(stream: PrintStream, event: Any, n: Int): Int =
  {
    if (event.isInstanceOf[Node]) {
      var node:Node = event.asInstanceOf[Node]
      var c:Any = node.getChildren.get(0)
      
      if (node.getChildren.size() == 1) {
         if (c.toString().length() > 8 && c.toString().substring(0, 8) == "NUMBER :" ) {
            stream.println("\t# loadI " +  c.toString().substring(9, c.toString().length()) + " => r" + registerIndex)
            stream.println("\tli $t0, " + c.toString().substring(9, c.toString().length()))
            var size = symbolIndexTable.size
            if (size + registerIndex > 0) 
              stream.println("\tsw $t0, -" + (size + registerIndex) * 4 + "($fp)")
            else   
              stream.println("\tsw $t0, 0($fp)")
            stream.println()  
            registerIndex = registerIndex + 1
  
            return symbolIndexTable.size + (registerIndex-1)
         }
         if (c.toString().length() > 7 && c.toString().substring(0, 7) == "IDENT :") {
              var variable: String = c.toString().substring(8, c.toString().length())
              var declType = symbolTable.get(variable) 
              return  symbolIndexTable.get(variable).get
         }
         if (c.toString().length() > 6 && c.toString().substring(0, 6) == "BOOL :") {
              return 0
         }
      } else {
          c = node.getChildren.get(1)
          printAsmExpression(stream, event, n)
      }
      
    }
    else {
      if (event.toString().length() > 8 && event.toString().substring(0, 8) == "NUMBER :" ) {
            stream.println("\t# loadI " +  event.toString().substring(9, event.toString().length()) + " => r" + registerIndex)
            stream.println("\tli $t0, " + event.toString().substring(9, event.toString().length()))
            var size = symbolIndexTable.size
            if (size + registerIndex > 0) 
              stream.println("\tsw $t0, -" + (size + registerIndex) * 4 + "($fp)")
            else   
              stream.println("\tsw $t0, 0($fp)")
            stream.println()  
            registerIndex = registerIndex + 1
           
            return symbolIndexTable.size + (registerIndex-1)
         }
         if (event.toString().length() > 7 && event.toString().substring(0, 7) == "IDENT :") {
              var variable: String = event.toString().substring(8, event.toString().length())
              var declType = symbolTable.get(variable) 
              return symbolIndexTable.get(variable).get
         }
         if (event.toString().length() > 6 && event.toString().substring(0, 6) == "BOOL :") {
              
              return 0
         }
    }  
    
    return 0
  }
  
  private def printAsmTerm(stream: PrintStream, event: Any, n: Int): Int =
  {
   if (event.isInstanceOf[Node])  {
      var node:Node = event.asInstanceOf[Node]
      var c:Any = node.getChildren.get(0)
      var returnType: Int = 0
      
      // factor
      if (node.getChildren.size() == 1) {
        return printAsmFactor(stream, c, n)
      } else {
        //myVar = myVar+1
        c = node.getChildren.get(0)
        var factor1  = printAsmFactor(stream, c, n)
        c = node.getChildren.get(2)
        var factor2 = printAsmFactor(stream, c, n)
        c = node.getChildren.get(1)
        if (c.toString() == "*") {
          var size = symbolIndexTable.size
          stream.print("\t# mult ")
          if (factor1 < size )
            stream.print("r_" + symbolArray(factor1))
          else 
            stream.print("r" + factor1)
          if (factor2 < size )
            stream.print(", r_" + symbolArray(factor2))
          else 
            stream.print(", r" + factor1)
          stream.println(" => r" +  registerIndex)
          if (factor1 > 0)
            stream.println("\tlw $t1, -" + (factor1*4) + "($fp)")
          else 
            stream.println("\tlw $t1, 0($fp)")
          if (factor2 > 0)
            stream.println("\tlw $t2, -" + (factor2*4) + "($fp)")
          else 
            stream.println("\tlw $t2, 0($fp)")  
          stream.println("\tmul $t0, $t1, $t2")
          
          if (size + registerIndex > 0) 
            stream.println("\tsw $t0, -" + (size+registerIndex)*4 + "($fp)")
          else   
            stream.println("\tsw $t0, 0($fp)")
          stream.println()  
          registerIndex = registerIndex + 1
        } else if (c.toString() == "/") {
          var size = symbolIndexTable.size
          stream.print("\t# dic ")
          if (factor1 < size )
            stream.print("r_" + symbolArray(factor1))
          else 
            stream.print("r" + factor1)
          if (factor2 < size )
            stream.print(", r_" + symbolArray(factor2))
          else 
            stream.print(", r" + factor1)
          stream.println(" => r" +  registerIndex)
          if (factor1 > 0)
            stream.println("\tlw $t1, -" + (factor1*4) + "($fp)")
          else 
            stream.println("\tlw $t1, 0($fp)")
          if (factor2 > 0)
            stream.println("\tlw $t2, -" + (factor2*4) + "($fp)")
          else 
            stream.println("\tlw $t2, 0($fp)")  
          stream.println("\tdiv $t0, $t1, $t2")
          if (size + registerIndex > 0) 
            stream.println("\tsw $t0, -" + (size+registerIndex)*4 + "($fp)")
          else   
            stream.println("\tsw $t0, 0($fp)")
          stream.println()
          
          registerIndex = registerIndex + 1
        }
        return symbolIndexTable.size + (registerIndex-1)
      } 
    } else {
      return printAsmFactor(stream, event, n)
    }
   return 0
  }
  
  private def printAsmSimpleExpression(stream: PrintStream, event: Any, n: Int): Int =
  {
    if (event.isInstanceOf[Node]) {
      var node:Node = event.asInstanceOf[Node]
      var c:Any = node.getChildren.get(0)
      var size = symbolIndexTable.size 
      
      // TERM
      if (node.getChildren.size() == 1) {
        return printAsmTerm(stream, c, n)
      } else {
        c = node.getChildren.get(0)
        var term = printAsmTerm(stream, c, n)
        c = node.getChildren.get(2)
        var expr = printAsmSimpleExpression(stream, c, n)
        c = node.getChildren.get(1)
        if (c.toString() == "+") {
          stream.print("\t# add ")
          if (term < size) 
            stream.print("r_" + symbolArray(term) + " , ")
          else 
            stream.print("r" + (term - size) + " , ")
          if (expr < size) 
            stream.print("r_" + symbolArray(expr))
          else 
            stream.print("r" + (expr - size))
          stream.println(" => r" + registerIndex)
          
          if (term > 0 ) 
            stream.println("\tlw $t1, -" + term*4 + "($fp)")
          else   
            stream.println("\tlw $t1, 0($fp)")
          if (expr> 0)
            stream.println("\tlw $t2, -" + expr*4 + "($fp)")
          else
            stream.println("\tlw $t2, 0($fp)")
          stream.println("\taddu $t0, $t1, $t2")
          if (registerIndex > 0)
            stream.println("\tsw $t0, -" + (registerIndex+size)*4 + "($fp)")
          else   
            stream.println("\tsw $t0, 0($fp)")
          stream.println()
          
        } else  if (c.toString() == "-") {
          stream.print("\t# sub ")
          if (term < size) 
            stream.print("r_" + symbolArray(term) + " , ")
          else 
            stream.print("r" + (term - size) + " , ")
          if (expr < size) 
            stream.print("r_" + symbolArray(expr))
          else 
            stream.print("r" + (expr - size))
          stream.println(" => r" + registerIndex)
          
          if (term > 0 ) 
            stream.println("\tlw $t1, -" + term*4 + "($fp)")
          else   
            stream.println("\tlw $t1, 0($fp)")
          if (expr> 0)
            stream.println("\tlw $t2, -" + expr*4 + "($fp)")
          else
            stream.println("\tlw $t2, 0($fp)")
          stream.println("\tsubu $t0, $t1, $t2")
          if (registerIndex > 0)
            stream.println("\tsw $t0, -" + (registerIndex+size)*4 + "($fp)")
          else   
            stream.println("\tsw $t0, 0($fp)")
          stream.println()
          
        }
        registerIndex = registerIndex + 1
        return (registerIndex - 1) + symbolIndexTable.size
      } 
    } else {
      return printAsmTerm(stream, event, n)
    } 
      
    return 0
  }
  
  private def printAsmExpression(stream: PrintStream, event: Any, n: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    
    var c:Any = node.getChildren.get(0)
    
    // <simpleExpression> case 
    if (node.getChildren.size() == 1) {
      c = node.getChildren.get(0)
      return printAsmSimpleExpression(stream, c, n)
    } else {
      // Add Compare to the tree, must be boolean
      c = node.getChildren.get(0)
      var simpleReturn = printAsmSimpleExpression(stream, c, n)
      c = node.getChildren.get(2)
      var expr = printAsmExpression(stream, c, n)
      
      c = node.getChildren.get(1)
      var size = symbolIndexTable.size
      if (c.toString() == "<") {
        stream.print("\t# cmp_LT ")
        if (simpleReturn < size)
          stream.print("r_" + symbolArray(simpleReturn) + ", ")
        else   
          stream.print("r" + (simpleReturn - size) + ", ")
        if (expr < size)
          stream.print("r_" + symbolArray(expr) + " => ")
        else   
          stream.print("r" + (expr - size) + " => ")  
        stream.println("r" + registerIndex)  
        if (simpleReturn > 0) 
          stream.println("\tlw $t1, -" + (simpleReturn*4) + "($fp)")
        else
          stream.println("\tlw $t1, 0($fp)")
        if (expr > 0) 
          stream.println("\tlw $t2, -" + (expr*4) + "($fp)")
        else
          stream.println("\tlw $t2, 0($fp)")
        stream.println("\tslt $t0, $t1, $t2")
        if (registerIndex > 0)
        {
          stream.println("\tsw $t0, -" + ((size+registerIndex)*4) + "($fp)")
        }
        else   
          stream.println("\tsw $t0, 0($fp)")
        stream.println()
      } else if (c.toString() == "<=") {
        stream.print("\t# cmp_LTE ")
        if (simpleReturn < size)
          stream.print("r_" + symbolArray(simpleReturn) + ", ")
        else   
          stream.print("r" + (simpleReturn - size) + ", ")
        if (expr < size)
          stream.print("r_" + symbolArray(expr) + " => ")
        else   
          stream.print("r" + (expr - size) + " => ")  
        stream.println("r" + registerIndex)  
        if (simpleReturn > 0) 
          stream.println("\tlw $t1, -" + (simpleReturn*4) + "($fp)")
        else
          stream.println("\tlw $t1, 0($fp)")
        if (expr > 0) 
          stream.println("\tlw $t2, -" + (expr*4) + "($fp)")
        else
          stream.println("\tlw $t2, 0($fp)")
        stream.println("\tsle $t0, $t1, $t2")
        if (registerIndex > 0)
        {
          stream.println("\tsw $t0, -" + ((size+registerIndex)*4) + "($fp)")
        }
        else   
          stream.println("\tsw $t0, 0($fp)")
        stream.println()  
      } else if (c.toString() == ">") {
        stream.print("\t# cmp_GT ")
        if (simpleReturn < size)
          stream.print("r_" + symbolArray(simpleReturn) + ", ")
        else   
          stream.print("r" + (simpleReturn - size) + ", ")
        if (expr < size)
          stream.print("r_" + symbolArray(expr) + " => ")
        else   
          stream.print("r" + (expr - size) + " => ")  
        stream.println("r" + registerIndex)  
        if (simpleReturn > 0) 
          stream.println("\tlw $t1, -" + (simpleReturn*4) + "($fp)")
        else
          stream.println("\tlw $t1, 0($fp)")
        if (expr > 0) 
          stream.println("\tlw $t2, -" + (expr*4) + "($fp)")
        else
          stream.println("\tlw $t2, 0($fp)")
        stream.println("\tsgt $t0, $t1, $t2")
        if (registerIndex > 0)
        {
          stream.println("\tsw $t0, -" + ((size+registerIndex)*4) + "($fp)")
        }
        else   
          stream.println("\tsw $t0, 0($fp)")
        stream.println() 
      } else if (c.toString() == ">=") {
        stream.print("\t# cmp_GTE ")
        if (simpleReturn < size)
          stream.print("r_" + symbolArray(simpleReturn) + ", ")
        else   
          stream.print("r" + (simpleReturn - size) + ", ")
        if (expr < size)
          stream.print("r_" + symbolArray(expr) + " => ")
        else   
          stream.print("r" + (expr - size) + " => ")  
        stream.println("r" + registerIndex)  
        if (simpleReturn > 0) 
          stream.println("\tlw $t1, -" + (simpleReturn*4) + "($fp)")
        else
          stream.println("\tlw $t1, 0($fp)")
        if (expr > 0) 
          stream.println("\tlw $t2, -" + (expr*4) + "($fp)")
        else
          stream.println("\tlw $t2, 0($fp)")
        stream.println("\tsge $t0, $t1, $t2")
        if (registerIndex > 0)
        {
          stream.println("\tsw $t0, -" + ((size+registerIndex)*4) + "($fp)")
        }
        else   
          stream.println("\tsw $t0, 0($fp)")
        stream.println()
      } else if (c.toString() == "=") {
        stream.print("\t# cmp_EQ ")
        if (simpleReturn < size)
          stream.print("r_" + symbolArray(simpleReturn) + ", ")
        else   
          stream.print("r" + (simpleReturn - size) + ", ")
        if (expr < size)
          stream.print("r_" + symbolArray(expr) + " => ")
        else   
          stream.print("r" + (expr - size) + " => ")  
        stream.println("r" + registerIndex)  
        if (simpleReturn > 0) 
          stream.println("\tlw $t1, -" + (simpleReturn*4) + "($fp)")
        else
          stream.println("\tlw $t1, 0($fp)")
        if (expr > 0) 
          stream.println("\tlw $t2, -" + (expr*4) + "($fp)")
        else
          stream.println("\tlw $t2, 0($fp)")
        stream.println("\tseq $t0, $t1, $t2")
        if (registerIndex > 0)
        {
          stream.println("\tsw $t0, -" + ((size+registerIndex)*4) + "($fp)")
        }
        else   
          stream.println("\tsw $t0, 0($fp)")
        stream.println()
        }
      
      registerIndex = registerIndex + 1 
      println("printAsmExpression return " + (registerIndex-1) + symbolIndexTable.size)
      return (registerIndex-1) + symbolIndexTable.size
    }
    
  }
  
  private def printAsmWriteInt(stream: PrintStream, event: Any, n: Int): Int = 
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    
    c = node.getChildren.get(1)
    var exprReturn = printAsmExpression(stream, c, n)
    
    var size = symbolIndexTable.size
    stream.print("\t# writeInt ")
    if (exprReturn < size)
      stream.println("r_" + symbolArray(exprReturn))
    else 
      stream.println("r" + exprReturn)
      
    stream.println("\tli $v0, 1")
    if (exprReturn > 0) 
      stream.println("\tlw $t1, -" + exprReturn*4+"($fp)")
    else 
      stream.println("\tlw $t1, 0($fp)")
      
    stream.println("\tadd $a0, $t1, $zero")
    stream.println("\tsyscall") 
    stream.println("\tli $v0, 4")
    stream.println("\tla $a0, newline")
    stream.println("\tsyscall")
    stream.println()
    return 0
  }
  
  private def printAsmWhileStatement(stream: PrintStream, event: Any, n: Int, parentNode: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
        
    if (n == 0) {
      exitNode = nodeCurrent
      println(" exitNode = " + exitNode)
    }
    // "WHILE" "EXPRESSION" "DO" "STATEMENTSEQUENT"
    c =  node.getChildren.get(1)
    var exprReturn = printAsmExpression(stream, c, n)
    
    
    var tempNode: Int = nodeCurrent
    var size = symbolIndexTable.size
    stream.print("\t# cbr ")
    if (exprReturn < size)
      stream.print("r_" + symbolArray(exprReturn) + " -> ")
    else
      stream.print("r" + (exprReturn-size) + " -> ")
    stream.println("B" + (nodeCurrent+1) + ", B" + (nodeCurrent + 2))
    if (exprReturn > 0)
      stream.println("\tlw $t0, -" + (exprReturn*4)+"($fp)")
    stream.println("\tbne $t0, $zero, B"  + (nodeCurrent+1))
    stream.println()
    stream.println("L" + lableL + ":")
    lableL = lableL + 1
    stream.println("\tj B" + (nodeCurrent+2))
    stream.println()
    
    stream.println("B" + (nodeCurrent+1) + ":")
    
    var nodeString: String = "B"+(nodeCurrent+1)
    nodeTable += (nodeString -> (nodeCurrent+1))
    
    nodeCurrent = nodeCurrent + 2
    nodeStep = 2
    c =  node.getChildren.get(3)
    var stateSeqReturn = printAsmStatementSequence(stream, c, n+1)
    
    stream.println("\t# jumpI  -> B" + tempNode)
    stream.println("\tj B" + tempNode)
    stream.println()
    
    nodeString = "B"+tempNode
    nodeString = "B"+(tempNode+2)
    nodeTable += (nodeString -> tempNode)
    
    println("JUMPI2 currentNode = " + nodeCurrent + ", B" + tempNode)
    println("JUMPI2 tempNode = " + tempNode + ", B" + tempNode)
    
    stream.println("B" + (tempNode+2) + ":")
    
    return (tempNode+2)
  }
  
  private def printAsmIfStatement(stream: PrintStream, event: Any, n: Int): Int =
  {
    return 0
  }
  private def printAsmStatement(stream: PrintStream, event: Any, n: Int): Int =
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    for (c <- node.getChildren) 
    {
      if (c.toString() == "ASSIGNMENT") {
        return printAsmAssignment(stream, c, n)
      } else if (c.toString() == "writeInt") {
        return printAsmWriteInt(stream, c, n)
      } else if (c.toString() == "WHILESTATEMENT") {
        var tempNode: Int = nodeCurrent
        
        stream.println("\t# jumpI  -> B" + (nodeCurrent+1))
        stream.println("\tj B" + (nodeCurrent+1))
        stream.println()
        stream.println("B" + (nodeCurrent+1) + ":") 
        
        var nodeString: String  = "B"+(nodeCurrent + 1)
        nodeTable += (nodeString -> nodeCurrent)
    
        nodeCurrent = nodeCurrent + 1
        nodeStep = 1
        
        var returnVal = printAsmWhileStatement(stream, c, n, nodeCurrent)
        println("while return = " + returnVal)
        return returnVal
        
      } else if (c.toString() == "IFSTATEMENT") {
        return printAsmIfStatement(stream, c, n)
      }
      
    }
    return 0
  }
  
  private def printAsmStatementSequence(stream: PrintStream, event: Any, n: Int)
  {
    var node:Node = event.asInstanceOf[Node]
    var c:Any = node.getChildren.get(0)
    var returnType: Int = 1
    if (c != Node.E) 
    {
      // "STATEMENT" ";" "STATEMENTSEQUENCE"
      c = node.getChildren.get(0)
      printAsmStatement(stream, c, n)
      
      c = node.getChildren.get(2)
      if (c != Node.E) {
        printAsmStatementSequence(stream, c, n) 
        
      }
        
    } 
  }
  
  private def printAsm(stream: PrintStream, event: Any, n: Int) 
  {
    var n1 = n
    var root = n1
    
    var node:Node = event.asInstanceOf[Node]
    var c: Any = node.getChildren.get(1)
    n1 =  DeclarationsGen(stream, c)
    
    c = node.getChildren.get(3)
    // First node in cluster (n0 node)
    stream.println("B" + (nodeCurrent+1)+ ":")
    nodeCurrent = nodeCurrent+1
    nodeStep = 1 
    
    for ((k,v)<- symbolIndexTable) {
      stream.println("\t# loadI 0 => r_"+ k)
      stream.println("\tli $t0, 0")
      if (v*4 > 0)
        stream.println("\tsw $t0, -" + v * 4 + "($fp)")
      else   
        stream.println("\tsw $t0, 0($fp)")
      stream.println()
    }
    printAsmStatementSequence(stream, c, root)
            
    // End of current node
    stream.println("\t# exit")
    stream.println("\tli $v0, 10")
    stream.println("\tsyscall")
    stream.println()
        
  }
    
}

class AsmGen(fileName: String) {

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
