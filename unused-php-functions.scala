import scala.collection.mutable.HashMap
import java.io.File
import java.util.regex._

case class FuncInfo(typeName: String, name: String, files: List[File]) {
  override def toString = typeName + " " + name + ": " + files.mkString(", ")
}

class UnusedPhpFunctions(rootDir: String) {

  def run = {
    val f = new File(rootDir)
    var funcs = readDefinitions(f)
    funcs = findUsedFuncs(f, funcs)
    report(funcs)
  }

  // Runs func for every line in every PHP file in and under dir.
  def eachPhpLine(dir: File, func: (File, String) => Unit) :Unit = {
    for (file <- dir.listFiles) {
      if (file.isDirectory()) eachPhpLine(file, func)
      else if (file.getName().endsWith(".php")) {
	for (line <- fileLines(file))
	  func(file, line.trim())
      }
    }
  }

  // Returns all lines in file f.
  def fileLines(f: File) = scala.io.Source.fromFile(f).getLines

  // Finds function and constant definitions. Returns a list of FuncInfo.
  def readDefinitions(dir: File) = {
    Console.withOut(System.err) { println("phase one") }
    var funcMap = new HashMap[String, FuncInfo]
    val funcRe = Pattern.compile("^\\s*function\\s*(\\w+)\\(")
    val descRe = Pattern.compile("^\\s*define\\s*\\(['\"]?(\\w+)")
    eachPhpLine(dir, (file: File, line: String) => {
      var m = funcRe.matcher(line)
      if (m.find())
	addDefinition(FuncInfo("func", m.group(1), List(file)), funcMap)
      else {
	m = descRe.matcher(line)
	if (m.find())
	  addDefinition(FuncInfo("def", m.group(1), List(file)), funcMap)
      }
    })
    funcMap.values.toList
  }

  def addDefinition(fi: FuncInfo, funcMap: HashMap[String, FuncInfo])  = {
    funcMap.get(fi.name) match {
      case Some(existingFileInfo) => 
	funcMap(fi.name) = FuncInfo(fi.typeName, fi.name,
				    fi.files.head :: existingFileInfo.files)
      case None =>
	funcMap += fi.name -> fi
    }
  }

  def findUsedFuncs(dir: File, funcs: List[FuncInfo]) = {
    Console.withOut(System.err) { println("phase two") }
    var (regexes, keys) = regexMap(funcs)
    Console.withOut(System.err) { println(keys.length) }
    eachPhpLine(dir, (file: File, line: String) => {
      var foundKeys = List[String]()
      for (key <- keys) {
	// if key found on line but it's not a declaration, remember the key
	val t = regexes(key)
	if (t._1.matcher(line).find() && !t._2.matcher(line).find())
	  foundKeys = key :: foundKeys
      }
      if (foundKeys.length > 0) {
	keys = keys.remove(key => (foundKeys.exists(_ == key)))
	Console.withOut(System.err) { println(keys.length) }
      }
    })
    // return only those funcs that are in the list of remaining not-found keys
    funcs.filter(f => (keys.exists(_ == f.name)))
  }

  def regexMap(funcs: List[FuncInfo]) = {
    var keys = List[String]()
    var regexes = new HashMap[String, (Pattern, Pattern)]
    for (func <- funcs) {
      val key = func.name
      keys = key :: keys
      val wordRegex = Pattern.compile("\\b" + key + "\\b")
      val defRegex =
	Pattern.compile("^\\s*(function\\s*|define\\s*\\(['\"]?)" + key)
      regexes += key -> (wordRegex, defRegex)
    }
    // TODO remove dupes from keys
    (regexes, keys)
  }

  def report(funcs: List[FuncInfo]) = funcs.map(println)
}

object Finder {
  def main(args: Array[String]) {
    val u = if (args.length == 0) new UnusedPhpFunctions(".")
	    else new UnusedPhpFunctions(args(0))
    u.run
  }
}
