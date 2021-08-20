package xyz.hyperreal.te

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Event {

  val events: mutable.Queue[Event]    = mutable.Queue()
  val timers: ArrayBuffer[Timeout]    = ArrayBuffer()
  val phases: ArrayBuffer[() => Unit] = ArrayBuffer()

  var running: Boolean                                     = _
  var reactions: ArrayBuffer[PartialFunction[Event, Unit]] = ArrayBuffer()
  var now: Long                                            = _

  def start(): Unit = {
    running = true

    while (running) {
      now = System.currentTimeMillis()

      get foreach { e =>
        reactions foreach { r =>
          if (r isDefinedAt e)
            r(e)
        }
      }

      timers.toList foreach {
        case t @ Timeout(expires, action) =>
          if (now >= expires) {
            action()
            cancel(t)
          }
      }

      phases foreach (_())
    }
  }

  def stop(): Unit = running = false

  def apply(e: Event): Unit = event(e)

  def phase(p: => Unit): Unit = phases += (() => p)

  def event(e: Event): Unit = synchronized(events enqueue e)

  def get: Option[Event] =
    if (events nonEmpty) Some(events.dequeue())
    else None

  def timeout(delay: Long)(action: => Unit): Timeout = {
    val t = Timeout(now + delay, () => action)

    timers += t
    t
  }

  def cancel(t: Timeout): Unit = timers -= t

}

case class Timeout(expires: Long, action: () => Unit)

trait Event
case class SegmentChangeEvent(views: Seq[TextView], line: Int, from: Int, count: Int, chars: String) extends Event
case class LineChangeEvent(views: Seq[TextView], line: Int, from: Int, chars: String)                extends Event
case class LinesChangeEvent(views: Seq[TextView], line: Int)                                         extends Event
case class DocumentModifiedEvent(model: TextModel)                                                   extends Event
case class DocumentLoadEvent(views: Seq[TextView])                                                   extends Event
case class DocumentSaveEvent(model: TextModel)                                                       extends Event
case class KeyEvent(key: String)                                                                     extends Event
case class MouseEvent(e: String)                                                                     extends Event
case object ResizeEvent                                                                              extends Event
