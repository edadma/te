package xyz.hyperreal.te

import java.io.PrintWriter
import java.util.NoSuchElementException
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TextModel(var path: String, init: String = null) {
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
      case DeleteAction(s, after)     => insert(after, s, noaction = true)
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
          char = eol(line)
          ('\n', char2col(line, char))
        } else
          throw new NoSuchElementException("no more characters to the left")
    } to LazyList

  def rightLazyList(p: Pos): LazyList[(Char, Pos)] =
    new Iterator[(Char, Pos)] {
      private var char = col2char(p)
      private var line = p.line

      def hasNext: Boolean = char < eol(line) || line < textBuffer.length - 1

      def next(): (Char, Pos) =
        if (char < eol(line)) {
          val res = (textBuffer(line)(char), char2col(line, char))

          char += 1
          res
        } else if (line < textBuffer.length - 1) {
          val res = ('\n', char2col(line, char))

          line += 1
          char = 0
          res
        } else
          throw new NoSuchElementException("no more characters to the right")
    } to LazyList
  def isWordChar(c: Char): Boolean = c.isLetterOrDigit || c == '_' | c == '$'

  def isDelimiter(c: Char): Boolean = "[]{}()" contains c

  def isSpace(c: Char): Boolean = " \t" contains c

  def isSymbol(c: Char): Boolean = !isWordChar(c) && !isSpace(c) && !isDelimiter(c) && c != '\n'

  def jumpLeft(l: LazyList[(Char, Pos)], move: Boolean): Option[Pos] =
    if (l.isEmpty) None
    else {
      val s =
        if (move) {
          l dropWhile { case (c, _) => isSpace(c) }
        } else {
          if (l.isEmpty || l.head._1 == '\n') l
          else {
            val a = l dropWhile { case (c, _) => isSpace(c) }

            if (a.nonEmpty && a.head._1 == '\n')
              return Some(l.drop((l takeWhile { case (c, _) => isSpace(c) }).length - 1).head._2)
            else a
          }
        }

      if (s.isEmpty) Some(Pos(0, 0))
      else {
        val c = s.head._1

        Some(
          if (isWordChar(c))
            (s takeWhile { case (c, _) => isWordChar(c) } last)._2
          else if (c == '\n')
            s.head._2
          else if (isDelimiter(c))
            (s takeWhile (_._1 == c) last)._2
          else
            (s takeWhile { case (c, _) => isSymbol(c) } last)._2)
      }
    }

  def jumpRight(l: LazyList[(Char, Pos)]): Option[Pos] = {
    if (l.isEmpty) None
    else {
      val s = l dropWhile { case (c, _) => isSpace(c) }

      Some(
        if (s.isEmpty) end
        else {
          val c = s.head._1
          val r =
            if (isWordChar(c)) {
              s dropWhile { case (c, _) => isWordChar(c) }
            } else if (c == '\n')
              s.tail
            else if (isDelimiter(c))
              s dropWhile (_._1 == c)
            else
              s dropWhile { case (c, _) => isSymbol(c) }

          if (r.isEmpty) end
          else r.head._2
        })
    }
  }

  def leftWord(p: Pos): Option[Pos] = jumpLeft(leftLazyList(p), move = true)

  def right(p: Pos): Option[Pos] = {
    val char         = col2char(p)
    val Pos(line, _) = p

    if (char < eol(line)) Some(char2col(line, char + 1))
    else if (line < textBuffer.length - 1) Some(Pos(line + 1, 0))
    else None
  }

  def rightWord(p: Pos): Option[Pos] = jumpRight(rightLazyList(p))

  def eol(line: Int): Int = textBuffer(line).length

  def chars(p1: Pos, p2: Pos): Int = {
    val List(a, b) = List(p1, p2).sorted

    if (a.line != b.line) {
      if (a.col == eol(a.line) && b.col == 0) 1
      else sys.error(s"chars: different by more than lf")
    } else col2char(b) - col2char(a)

  }

  def backspace(p: Pos): Option[Pos] = left(p) map (np => delete(np, 1))

  def backspaceWord(p: Pos): Option[Pos] = jumpLeft(leftLazyList(p), move = false) map (np => delete(np, chars(p, np)))

  def deleteWord(p: Pos): Option[Pos] = jumpRight(rightLazyList(p)) map (np => delete(p, chars(p, np)))

  def views: List[TextView] = subscribers.toList

  def slice(line: Int, from: Int, until: Int): String = textBuffer(line).slice(from, until).mkString

  def slice(line: Int, from: Int): String = slice(line, from, eol(line))

  var autosaveTimer: Timeout = _

  def modified(): Unit = {
    if (autosaveTimer ne null)
      Event.cancel(autosaveTimer)

    autosaveTimer = Event.timeout(5 * 1000) { save() }
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
    Event.cancel(autosaveTimer)
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
                      if (eol(line) > char) textBuffer(line).slice(char, eol(line))
                      else new ArrayBuffer[Char])

    if (eol(line) > char) {
      textBuffer(line).remove(char, eol(line) - char)
      Event(LinesChangeEvent(views, line))
    } else
      Event(LinesChangeEvent(views, line + 1))

    modified()

    val after = Pos(line + 1, 0)

    if (noaction) after
    else action(InsertAction(1, p, after))
  }

  def insert(p: Pos, s: String, noaction: Boolean = false): Pos = {
    val char           = col2char(p)
    val Pos(line, col) = p

    textBuffer(line).insertAll(char, s)
    Event(LineChangeEvent(views, line, col, slice(line, char)))
    modified()

    val after = Pos(line, col + s.length)

    if (noaction) after
    else action(InsertAction(s.length, p, after))
  }

  def delete(p: Pos, n: Int, noaction: Boolean = false): Pos = {
    val char           = col2char(p)
    val Pos(line, col) = p
    val a =
      if (char < eol(line)) {
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
