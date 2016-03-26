import java.io.File

import akka.actor.{Actor, ActorSystem, Props}

import scala.io.Source

/**
  * Created by Domenico on 03/12/2015.
  */
case class Path(file: File)

case class Line(file: File, line: String, index: Int)

case class Directory(file: File)

case object Done

object Main {
  var fileCount: Int = 0

  def getRecursiveListOfFiles(dir: File): Array[File] = {
    val these = dir.listFiles
    these ++ these.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
  }

  var matcher = ""

  def main(args: Array[String]): Unit = {
    if (args.length < 2)
      return

    val system = ActorSystem("Main")
    val fileScanner = system.actorOf(Props(new FileScanner), "FileScanner")
    matcher = args(0)
    val filePath = args(1)
    val file = new File(filePath)
    val files = getRecursiveListOfFiles(file).filter(f => f.isFile && !f.getName.endsWith(".lnk"))
    fileCount = files.length
    files.foreach(f => fileScanner ! Path(f))
    system.awaitTermination()
  }
}


class FileScanner extends Actor {

  val lineScanner = context.system.actorOf(Props(new LineScanner), "LineScanner")
  var currLines = 0
  var totalLines = 0
  var fileCount = 0

  override def receive: Receive = {
    case f: Path =>
      println(f.file)
      totalLines = Source.fromFile(f.file).getLines.length
      for ((line, index) <- Source.fromFile(f.file).getLines.zipWithIndex) {
        lineScanner ! Line(f.file, line, index + 1)
      }
    case Done =>
      currLines += 1
      if (currLines == totalLines) {
        fileCount += 1
      }
      if (fileCount == Main.fileCount) {
        context.system.shutdown()
      }
  }
}

class LineScanner extends Actor {
  override def receive: Receive = {
    case line: Line =>
      if (line.line.contains(Main.matcher)) {
        println(s"${line.file}:${line.index}:${line.line}")
      }
      sender ! Done
  }
}
