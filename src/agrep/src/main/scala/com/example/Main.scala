package com.example

import java.io.File
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{ActorSystem, Props}

import scala.concurrent.duration.Duration

/**
  * Created by Domenico on 03/12/2015.
  */
case class Path(file: File)

case class Line(file: String, line: String, index: Int)

case class Directory(file: File)

case object Done

object Main {
  var fileCount: Int = 0
  var matches = new AtomicInteger(0)

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
    val files = getRecursiveListOfFiles(file).filter(f => f.isFile)
    fileCount = files.length
    files.foreach(f => fileScanner ! Path(f))
    try {
      system.awaitTermination(Duration.apply("500ms"))
    } catch {
      case _: Exception =>
        system.shutdown()
    } finally {
      println(s"Total matches: ${matches.get()}")
    }
  }
}





