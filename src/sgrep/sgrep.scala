
object SimpleGrep {
   def main(argc: Array[String])
   {
    def matchType(list: String) : List[String] = list match {
        case "code"   => List(".c", ".cpp", ".cc", ".h", ".hpp", ".java", ".cs", ".s")
        case "res"    => List(".xml", ".rc", ".xaml")
        case "mk"     => List(".mk", "Makefile", ".in", "make")
        case "script"   => List(".sh", ".py", ".pl")
        case "any"    => List("any")
        case _      => List(list)
      }
      
    def fileLines(file: java.io.File) = scala.io.Source.fromFile(file).getLines.toList
    
    if(argc.length == 0) {
      println("Usage: grep <pattern> <type> <path>")
      println("type and path are optional")
      return
    }
    
    def findRecursive(file : java.io.File) : Array[java.io.File] = {
      val files = file.listFiles
      files ++ files.filter(_.isDirectory).flatMap(findRecursive)
    }

    val extensions : List[String] = if (argc.length > 1) matchType(argc(1)) else matchType("any")
    val files = findRecursive(new java.io.File(if (argc.length > 2) argc(2) else "."))
    for (file <- files
      if file.isFile;
      extension <- extensions
      if file.getName.endsWith(extension) || extension == "any";
      line <- fileLines(file)
      if line.contains(argc(0))) 
        println(file + ":" + (fileLines(file).indexOf(line) + 1) + "  " + line)
  }
}
