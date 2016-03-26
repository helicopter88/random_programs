package com.example

import akka.actor.Actor

/**
  * Created by domenico on 26/03/16.
  */
class LineScanner extends Actor {
  override def receive: Receive = {
    case line: Line =>
      if (line.line.contains(Main.matcher)) {
        println(s"${line.file}:${line.index}:${line.line}")
        Main.matches.addAndGet(1)
      }
      sender ! Done
  }
}
