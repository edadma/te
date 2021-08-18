package xyz.hyperreal.te

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer

class TextModel(val path: String, init: String = null) {
  val text = new ArrayBuffer[ArrayBuffer[Char]]()

  if (init == null)
    text += new ArrayBuffer[Char]
  else {
    for (l <- io.Source.fromString(init).getLines())
      text += (ArrayBuffer[Char]() ++ l)

    if (text.isEmpty)
      text += new ArrayBuffer[Char]
  }

  val subscribers = new ArrayBuffer[TextView]

  var exptabs = false
  var tabs    = 2

  def subscribe(view: TextView): Unit = subscribers += view

  def unsubscribe(view: TextView): Unit = subscribers -= view

  def lines: Int = text.length

  def char2col(line: Int, char: Int): Pos = {
    var col = 0
    val s   = text(line)

    for (i <- 0 until char)
      col += (if (s(i) == '\t') tabs - col % tabs else 1)

    Pos(line, col)
  }

  def col2char(p: Pos): Int = {
    var char = 0
    var cur  = 0
    val s    = text(p.line)

    while (cur < p.col && char < s.length) {
      cur += (if (s(char) == '\t') tabs - cur % tabs else 1)
      char += 1
    }

    char
  }

  def up(p: Pos, n: Int): Option[Pos] = {
    val Pos(line, _) = p
    val dist         = n min line

    if (dist > 0) Some(char2col(line - dist, col2char(p.copy(line = line - dist))))
    else None
  }

  def down(p: Pos, n: Int): Option[Pos] = {
    val Pos(line, _) = p
    val dist         = n min (text.length - 1 - line)

    if (dist > 0) Some(char2col(line + dist, col2char(p.copy(line = line + dist))))
    else None
  }

  def startOfLine(p: Pos): Option[Pos] =
    if (p.col > 0) Some(p.copy(col = 0))
    else None

  def endOfLine(p: Pos): Option[Pos] = {
    val endp = char2col(p.line, text(p.line).length)

    if (p.col < endp.col) Some(endp)
    else None
  }

  def end: Pos = char2col(text.length - 1, text(text.length - 1).length)

  def left(p: Pos): Option[Pos] = {
    val char         = col2char(p)
    val Pos(line, _) = p

    if (char > 0) Some(char2col(line, char - 1))
    else if (line > 0) Some(char2col(line - 1, text(line - 1).length))
    else None
  }

  def right(p: Pos): Option[Pos] = {
    val char         = col2char(p)
    val Pos(line, _) = p

    if (char < text(line).length) Some(char2col(line, char + 1))
    else if (line < text.length - 1) Some(Pos(line + 1, 0))
    else None
  }

  def backspace(p: Pos): Option[Pos] = left(p) map delete

  def views: List[TextView] = subscribers.toList

  def delete(p: Pos): Pos = {
    val char           = col2char(p)
    val Pos(line, col) = p

    if (char < text(line).length) {
      text(line).remove(char)
      Event(LineChangeEvent(views, line, col, slice(line, char)))
      modified()
    } else if (line < text.length - 1) {
      text(line).addAll(text(line + 1))
      text.remove(line + 1)
      Event(LinesChangeEvent(views, line))
      modified()
    }

    p
  }

  def slice(line: Int, from: Int, until: Int): String = text(line).slice(from, until).mkString

  def slice(line: Int, from: Int): String = slice(line, from, text(line).length)

  var autosaveTimer: Timeout = _

  def modified(): Unit = {
    if (autosaveTimer ne null)
      Event.cancel(autosaveTimer)

    autosaveTimer = Event.timeout(2 * 1000) { save() }
    Event(DocumentModifiedEvent(this))
  }

  def save(): Unit = {
    val w = new PrintWriter(path)

    for (l <- text)
      w.println(l.mkString)

    w.close()
    Event(NotificationEvent(s""""$path" saved"""))
  }

  def insertTab(p: Pos): Pos = {
    val char           = col2char(p)
    val Pos(line, col) = p
    val spaces         = tabs - col % tabs

    text(line).insertAll(char, if (exptabs) " " * spaces else "\t")
    Event(LineChangeEvent(views, line, col, slice(line, char)))
    modified()
    Pos(line, col + spaces)
  }

  def insertBreak(p: Pos): Pos = {
    val char         = col2char(p)
    val Pos(line, _) = p

    text.insert(line + 1,
                if (text(line).length > char) text(line).slice(char, text(line).length)
                else new ArrayBuffer[Char])

    if (text(line).length > char) {
      text(line).remove(char, text(line).length - char)
      Event(LinesChangeEvent(views, line))
    } else
      Event(LinesChangeEvent(views, line + 1))

    modified()
    Pos(line + 1, 0)
  }

  def insert(p: Pos, c: Char): Pos = {
    val char           = col2char(p)
    val Pos(line, col) = p

    text(line).insert(char, c)
    Event(LineChangeEvent(views, line, col, slice(line, char)))
    modified()
    Pos(line, col + 1)
  }

  def getLine(line: Int): String = text(line).mkString

  /*
          Event.timeout(2 * 1000) {
          status()
          view.cursor(pos)
        }

 */
}
