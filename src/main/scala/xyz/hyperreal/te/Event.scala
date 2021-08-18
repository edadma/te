package xyz.hyperreal.te

trait Event
case class SegmentChange(line: Int, from: Int, count: Int, chars: String) extends Event
case class LineChange(line: Int, from: Int, chars: String)                extends Event
case class DocumentChange(line: Int)                                      extends Event
