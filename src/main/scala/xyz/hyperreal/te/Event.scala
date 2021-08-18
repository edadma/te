package xyz.hyperreal.te

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Event {

  val events: mutable.Queue[Event]  = mutable.Queue()
  val loop: ArrayBuffer[() => Unit] = ArrayBuffer()

  var running: Boolean       = _
  var handler: Event => Unit = _

  phase {
    get foreach handler
  }

  def start(): Unit = {
    running = true
    while (running) loop foreach (_())
  }

  def stop(): Unit = running = false

  def apply(e: Event): Unit = event(e)

  def phase(p: => Unit): Unit = loop += (() => p)

  def event(e: Event): Unit = events enqueue e

  def get: Option[Event] =
    if (events nonEmpty) Some(events.dequeue())
    else None

}

trait Event
case class SegmentChangeEvent(views: Seq[TextView], line: Int, from: Int, count: Int, chars: String) extends Event
case class LineChangeEvent(views: Seq[TextView], line: Int, from: Int, chars: String)                extends Event
case class DocumentChangeEvent(views: Seq[TextView], line: Int)                                      extends Event
