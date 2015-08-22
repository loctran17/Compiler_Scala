import java.util.ArrayList
import Node._
import NodeType._

import scala.collection.JavaConversions._

object Node {

  val E = new Node(NodeType.E)
}

class Node(private var nodeType: NodeType) {

  private var nodes: ArrayList[Node] = new ArrayList[Node]()

  private var tokens: ArrayList[Token] = new ArrayList[Token]()

  private var objects: ArrayList[Any] = new ArrayList[Any]()

  def getChildren(): ArrayList[Any] = this.objects

  def addNode(node: Node) {
    this.nodes.add(node)
    this.objects.add(node)
  }

  def addToken(token: Token) {
    println("Inside addToken, " + token.toString())
    this.tokens.add(token)
    this.objects.add(token)
  }

  override def toString(): String ={
    this.nodeType.toString
  } 
}
