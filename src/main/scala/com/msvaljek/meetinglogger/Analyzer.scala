package com.msvaljek.meetinglogger

import scala.io.Source

object Analyzer extends App {
  val fileName = "/tmp/file.out"

  val source = Source.fromFile(fileName)

  var participants = List.empty[Participant]

  var previousLogLine: Option[LogLine] = None

  for (line <- source.getLines()) {
    val logLine = LogLine.fromLine(line)

    if (!logLine.isHeader) {
      previousLogLine match {
        case Some(previous) =>
          val addedTalkTime = logLine.timestamp - previous.timestamp

          participants.find(_.marker == previous.marker) match {
            case None =>
              val newParticipant = Participant(
                marker = logLine.marker,
                totalTalkTime = 0,
                wordTaken = Map.empty
              )

              participants = participants :+ newParticipant

            case Some(previous) =>
              val updatedTalkTime = previous.copy(totalTalkTime = previous.totalTalkTime + addedTalkTime)

              val newWordTaken = previous.wordTaken.get(logLine.marker) match {
                case None =>
                  1
                case Some(existingWordTaken) =>
                  existingWordTaken + 1
              }

              val updatedWordTaken = updatedTalkTime.copy(wordTaken = previous.wordTaken + (logLine.marker -> newWordTaken))

              participants = participants.filterNot(_.marker == previous.marker) :+ updatedWordTaken

              if (!participants.exists(_.marker == logLine.marker)) {
                val newParticipant = Participant(
                  marker = logLine.marker,
                  totalTalkTime = 0,
                  wordTaken = Map.empty
                )

                participants = participants :+ newParticipant
              }
          }
        case None =>
          val newParticipant = Participant(
            marker = logLine.marker,
            totalTalkTime = 0,
            wordTaken = Map.empty
          )

          participants = participants :+ newParticipant
      }

      previousLogLine = Some(logLine)
    }
  }

  participants.foreach(println)

  source.close()
}

final case class Participant(marker: LogLine.Marker, totalTalkTime: Long, wordTaken: Map[LogLine.Marker, Long])

final case class LogLine(timestamp: Long, marker: LogLine.Marker, isLast: Boolean = false, isHeader: Boolean = false)

case object LogLine {

  type Marker = String

  val header = "timestamp,marker"

  val endMarker = "END_MARKER"

  def fromLine(in: String): LogLine = {
    val elements = in.split(",")

    if (in == header)
      LogLine(
        timestamp = 0,
        marker = "",
        isHeader = true
      )
    else
      LogLine(
        timestamp = elements(0).toLong,
        marker = elements(1),
        isLast = elements(1) == endMarker
      )
  }
}
