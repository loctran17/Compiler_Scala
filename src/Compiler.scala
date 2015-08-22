import java.io.IOException


object Compiler 
{

  val TL15 = "tl"

  val PTDOT = "pt.dot"
  
  val ASTDOT = "ast.dot"
  
  val CFGDOT = "3A.cfg.dot"
  
  val ASMDOT = "s"
  
  val ParseTree = ".dot"

  def main(args: Array[String]) {
    if (args.length == 1) {
      var fileName = args(0)
      //fileName = fileName.replaceFirst(TL15, ParseTree)
      //val outFileName = fileName.replaceFirst(TL15, PTDOT)
      val ptFileName = fileName.replaceFirst(TL15, PTDOT)
      val astFileName = fileName.replaceFirst(TL15, ASTDOT)
      val cfgFileName = fileName.replaceFirst(TL15, CFGDOT)
      val asmFileName = fileName.replaceFirst(TL15, ASMDOT)

      try {
        Parser.toDotFile(ptFileName, new Parser(args(0)).parse())
        
        if (TypeCheckParser.toDotFile(astFileName, new Parser(args(0)).parse()) != 1) {
            ASTParser.toDotFile(astFileName, new Parser(args(0)).parse())
            CfgGen.toDotFile(cfgFileName, new Parser(args(0)).parse())
            AsmGen.toDotFile(asmFileName, new Parser(args(0)).parse())
        }
      } catch {
        case ex: IOException => System.err.println(ex.getMessage)
        case pe: ParserError => System.err.println("PARSER  ERROR " + pe.getMessage)
      }
    }
  }
}
