package xyz.hyperreal.te

import java.io.PrintWriter
import java.util.NoSuchElementException
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TextModel(val path: String, init: String = null) {
  val textBuffer = new ArrayBuffer[ArrayBuffer[Char]]()
  val undoBuffer = new mutable.Stack[Action]

  abstract class Action { val after: Pos }
  case class InsertAction(n: Int, before: Pos, after: Pos) extends Action
  case class DeleteAction(s: String, after: Pos)           extends Action
  case class DeleteBreakAction(after: Pos)                 extends Action
  case class DeleteTabAction(after: Pos)                   extends Action
  case class ReplaceAction(s: String, after: Pos)          extends Action

  val subscribers = new ArrayBuffer[TextView]

  var exptabs = false
  var tabs    = 2
  var unsaved = false

  if (init == null)
    textBuffer += new ArrayBuffer[Char]
  else {
    for (l <- io.Source.fromString(init).getLines())
      textBuffer += (ArrayBuffer[Char]() ++ l)

    if (textBuffer.isEmpty)
      textBuffer += new ArrayBuffer[Char]
  }

  def afterLast: Option[Pos] =
    if (undoBuffer.nonEmpty) Some(undoBuffer.top.after)
    else None

  def action(a: Action): Pos = {
    undoBuffer push a
    a.after
  }

  def undo: Pos =
    undoBuffer.pop() match {
      case InsertAction(n, before, _) => delete(before, n, noaction = true)
      case DeleteAction(s, after)     => insert(after, s.head, noaction = true)
      case DeleteBreakAction(after)   => insertBreak(after, noaction = true)
      case DeleteTabAction(after)     => insertTab(after, noaction = true)
    }

  def subscribe(view: TextView): Unit = subscribers += view

  def unsubscribe(view: TextView): Unit = subscribers -= view

  def lines: Int = textBuffer.length

  def char2col(line: Int, char: Int): Pos = {
    var col = 0
    val s   = textBuffer(line)

    for (i <- 0 until char)
      col += (if (s(i) == '\t') tabs - col % tabs else 1)

    Pos(line, col)
  }

  def col2char(p: Pos): Int = {
    var char = 0
    var cur  = 0
    val s    = textBuffer(p.line)

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
    val dist         = n min (textBuffer.length - 1 - line)

    if (dist > 0) Some(char2col(line + dist, col2char(p.copy(line = line + dist))))
    else None
  }

  def startOfLine(p: Pos): Option[Pos] =
    if (p.col > 0) Some(p.copy(col = 0))
    else None

  def endOfLine(p: Pos): Option[Pos] = {
    val endp = char2col(p.line, textBuffer(p.line).length)

    if (p.col < endp.col) Some(endp)
    else None
  }

  def end: Pos = char2col(textBuffer.length - 1, textBuffer(textBuffer.length - 1).length)

  def left(p: Pos): Option[Pos] = {
    val char         = col2char(p)
    val Pos(line, _) = p

    if (char > 0) Some(char2col(line, char - 1))
    else if (line > 0) Some(char2col(line - 1, textBuffer(line - 1).length))
    else None
  }

  def leftLazyList(p: Pos): LazyList[(Char, Pos)] =
    new Iterator[(Char, Pos)] {
      private var char = col2char(p)
      private var line = p.line

      def hasNext: Boolean = char > 0 || line > 0

      def next(): (Char, Pos) =
        if (char > 0) {
          char -= 1
          (textBuffer(line)(char), char2col(line, char))
        } else if (line > 0) {
          line -= 1
          char = textBuffer(line).length
          ('\n', char2col(line, char))
        } else
          throw new NoSuchElementException("no more characters to the left")
    } to LazyList

  def isWordChar(c: Char): Boolean = c.isLetterOrDigit || c == '_' | c == '$'

  def isDelimiter(c: Char): Boolean = "[]{}()" contains c

  def isSpace(c: Char): Boolean = " \t" contains c

  def isSymbol(c: Char): Boolean = !isWordChar(c) && !isSpace(c) && !isDelimiter(c) && c != '\n'

  def jump(l: LazyList[(Char, Pos)]): Pos = {
    var s = l dropWhile { case (c, _) => isSpace(c) }

    val c = s.head._1

    if (isWordChar(c))
      (s takeWhile { case (c, _) => isWordChar(c) } last)._2
    else if (c == '\n')
      s.tail.head._2
    else if (isDelimiter(c))
      (s takeWhile (_._1 == c) last)._2
    else
      (s takeWhile { case (c, _) => isSymbol(c) } last)._2
  }

  def leftWord(p: Pos): Pos = jump(leftLazyList(p))

  def right(p: Pos): Option[Pos] = {
    val char         = col2char(p)
    val Pos(line, _) = p

    if (char < textBuffer(line).length) Some(char2col(line, char + 1))
    else if (line < textBuffer.length - 1) Some(Pos(line + 1, 0))
    else None
  }

  def backspace(p: Pos): Option[Pos] = left(p) map (np => delete(np, 1))

  def views: List[TextView] = subscribers.toList

  def slice(line: Int, from: Int, until: Int): String = textBuffer(line).slice(from, until).mkString

  def slice(line: Int, from: Int): String = slice(line, from, textBuffer(line).length)

  var autosaveTimer: Timeout = _

  def modified(): Unit = {
    if (autosaveTimer ne null)
      Event.cancel(autosaveTimer)

    autosaveTimer = Event.timeout(2 * 1000) { save() }
    Event(DocumentModifiedEvent(this))
    unsaved = true
  }

  def save(): Unit = {
    val w = new PrintWriter(path, "UTF-8")

    for (l <- textBuffer)
      w.println(l.mkString)

    w.close()
    Event(DocumentSaveEvent(this))
    unsaved = false
  }

  def insertTab(p: Pos, noaction: Boolean = false): Pos = {
    val char           = col2char(p)
    val Pos(line, col) = p
    val spaces         = tabs - col % tabs
    val chars          = if (exptabs) " " * spaces else "\t"

    textBuffer(line).insertAll(char, chars)
    Event(LineChangeEvent(views, line, col, slice(line, char)))
    modified()

    val after = Pos(line, col + spaces)

    if (noaction) after
    else action(InsertAction(chars.length, p, after))
  }

  def insertBreak(p: Pos, noaction: Boolean = false): Pos = {
    val char         = col2char(p)
    val Pos(line, _) = p

    textBuffer.insert(line + 1,
                      if (textBuffer(line).length > char) textBuffer(line).slice(char, textBuffer(line).length)
                      else new ArrayBuffer[Char])

    if (textBuffer(line).length > char) {
      textBuffer(line).remove(char, textBuffer(line).length - char)
      Event(LinesChangeEvent(views, line))
    } else
      Event(LinesChangeEvent(views, line + 1))

    modified()

    val after = Pos(line + 1, 0)

    if (noaction) after
    else action(InsertAction(1, p, after))
  }

  def insert(p: Pos, c: Char, noaction: Boolean = false): Pos = {
    val char           = col2char(p)
    val Pos(line, col) = p

    textBuffer(line).insert(char, c)
    Event(LineChangeEvent(views, line, col, slice(line, char)))
    modified()

    val after = Pos(line, col + 1)

    if (noaction) after
    else action(InsertAction(1, p, after))
  }

  def delete(p: Pos, n: Int, noaction: Boolean = false): Pos = {
    val char           = col2char(p)
    val Pos(line, col) = p
    val a =
      if (char < textBuffer(line).length) {
        val s = textBuffer(line).slice(char, char + n).mkString

        textBuffer(line).remove(char, n)
        Event(LineChangeEvent(views, line, col, slice(line, char)))
        modified()

        if (s.head == '\t') DeleteTabAction(p)
        else DeleteAction(s, p)
      } else if (line < textBuffer.length - 1) {
        require(n == 1, s"delete: since a line break is being removed, n should be 1: n = $n")
        textBuffer(line).addAll(textBuffer(line + 1))
        textBuffer.remove(line + 1)
        Event(LinesChangeEvent(views, line))
        modified()
        DeleteBreakAction(p)
      } else null

    if (noaction || a == null) p
    else action(a)
  }

  def getLine(line: Int): String = textBuffer(line).mkString

  /*
          Event.timeout(2 * 1000) {
          status()
          view.cursor(pos)
        }

 */
}
