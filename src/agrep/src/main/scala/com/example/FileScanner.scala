package com.example

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{Actor, Props}

import scala.io.Source

/**
  * Created by domenico on 26/03/16.
  */
class FileScanner extends Actor {
  var currLines = new AtomicInteger(0)
  var totalLines = 0
  var fileCount: AtomicInteger = new AtomicInteger(0)
  override def receive: Receive = {
    case f: Path =>
      try {
      totalLines = Source.fromFile(f.file).getLines.length
        val lineScanner = context.system.actorOf(Props(new LineScanner), "LineScanner" + f.file.getName)
        if (totalLines >= 2) {
          for ((line, index) <- Source.fromFile(f.file).getLines.zipWithIndex) {
            lineScanner ! Line(f.file.getName, line, index + 1)
          }
        }
      } catch {
        case e: Exception => fileCount.addAndGet(1)
          //context.system.dispatcher.reportFailure(e)
      }
    case Done =>
      if (currLines.addAndGet(1) == totalLines) {
        fileCount.addAndGet(1)
        currLines.set(0)
      }
      if(fileCount.get() == Main.fileCount) {
        context.system.shutdown()
      }
  }
}
