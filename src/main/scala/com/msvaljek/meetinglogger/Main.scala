package com.msvaljek.meetinglogger

import java.io.{BufferedWriter, File, FileWriter}
import util.Properties

object Main extends App {

  val fileName = "/tmp/file.out"

  def timestamp: Long = System.currentTimeMillis / 1000

  val newLine = Properties.lineSeparator

  val bw = new BufferedWriter(new FileWriter(new File(fileName), false))

  bw.write( LogLine.header + newLine)

  Iterator.continually(io.StdIn.readLine)
    .takeWhile(_ != "exit")
    .foreach {
      case line: String =>
        bw.write(s"$timestamp,$line" + newLine)
    }

  bw.write(s"$timestamp,${LogLine.endMarker}" + newLine)

  bw.close
}
