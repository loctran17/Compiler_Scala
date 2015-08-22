
import scala.collection.JavaConversions._

object NodeType extends Enumeration {

  val program = new NodeType()

  val DECLARATIONS = new NodeType()

  val TYPE = new NodeType()

  val STATEMENTSEQUENCE = new NodeType()

  val STATEMENT = new NodeType()

  val ASSIGNMENT = new NodeType()

  val IFSTATEMENT = new NodeType()

  val WHILESTATEMENT = new NodeType()

  val writeInt = new NodeType()

  val ELSECLAUSE = new NodeType()

  val EXPRESSION = new NodeType()

  val SIMPLEEXPRESSION = new NodeType()

  val TERM = new NodeType()

  val FACTOR = new NodeType()

  val E = new NodeType()

  class NodeType extends Val

  implicit def convertValue(v: Value): NodeType = v.asInstanceOf[NodeType]
}
