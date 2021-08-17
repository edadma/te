package xyz.hyperreal.te

import xyz.hyperreal.ncurses.LibNCurses._
import xyz.hyperreal.ncurses.LibNCursesHelpers._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.scalanative.unsafe.{Zone, toCString}

object Main extends App {

  initscr
  cbreak
  noecho

  val testdoc =
    """
      |one
      |two
      |three
      |four
      |five
      |six
      |seven
      |eight
      |nine
      |ten
      |""".trim.stripMargin

  val HOME     = Pos(0, 0)
  val view     = new TextView(new TextModel(testdoc), getmaxy(stdscr) - 3, getmaxx(stdscr), 2, 0)
  var pos: Pos = _

  def home(): Unit = cursor(HOME)

  def cursor(p: Pos): Unit = Zone { implicit z =>
    pos = p
    move(getmaxy(stdscr) - 1, 0)
    wbkgdset(stdscr, ' ' | A_REVERSE | A_DIM)

    val status = s"${p.line + 1}:${p.col + 1}  LF  UTF-8  2-spaces"

    clrtoeol
    move(getmaxy(stdscr) - 1, getmaxx(stdscr) - status.length)
    addstr(toCString(status))
    wbkgdset(stdscr, ' ')
    refresh
    view.cursor(p)
  }

  @tailrec
  def listen(): Unit = {
    val c = wgetch(view.win)

    if (c == KEY_HOME)
      view.model.start(pos) foreach cursor
    else if (c == KEY_END)
      view.model.end(pos) foreach cursor
    else if (c == KEY_PPAGE)
      view.model.up(pos, view.height) foreach cursor
    else if (c == KEY_NPAGE)
      view.model.down(pos, view.height) foreach cursor
    else if (c == KEY_UP)
      view.model.up(pos, 1) foreach cursor
    else if (c == KEY_DOWN)
      view.model.down(pos, 1) foreach cursor
    else if (c == KEY_LEFT)
      view.model.left(pos) foreach cursor
    else if (c == KEY_RIGHT) {
      view.model.right(pos) foreach cursor
    } else if (c == KEY_BACKSPACE) {
      view.model.backspace(pos) foreach cursor
    } else if (c == KEY_DC)
      view.cursor(view.model.delete(pos))
    else
      cursor(view.model.insert(pos, c.toChar))

    listen()
  }

  keypad(view.win, bf = true)
  home()
  listen()
  endwin

}

case class Pos(line: Int, col: Int)

trait Event
case class SegmentChange(line: Int, from: Int, count: Int, chars: String) extends Event
case class LineChange(line: Int, from: Int, chars: String)                extends Event
case class DocumentChange(line: Int)                                      extends Event

class TextView(val model: TextModel, nlines: Int, val ncols: Int, begin_y: Int, begin_x: Int) {
  val win: WINDOW = newwin(nlines, ncols, begin_y, begin_x)

  model subscribe this

  var top: Int = _

  viewport(0)

  def react(e: Event): Unit = Zone { implicit z =>
    e match {
      case DocumentChange(line) => render(visibleFrom(line))
      case LineChange(line, from, chars) =>
        if (visibleLine(line))
          render(line, from, chars)
      //case SegmentChange(line, from, count, chars) =>
    }
  }

  def viewport(from: Int): Unit = {
    top = from
    render(from until ((from + height) min model.lines))
    wclrtobot(win)
  }

  def render(line: Int, from: Int, chars: String): Unit = Zone { implicit z =>
    wmove(win, line - top, from)
    waddstr(win, toCString(chars))
    wclrtoeol(win)
  }

  def render(range: Seq[Int]): Unit =
    for (i <- range)
      render(i, 0, model.getLine(i))

  def height: Int = getmaxy(win)

  def visibleLine(line: Int): Boolean = top <= line && line < top + height

  def visibleFrom(line: Int): Seq[Int] = line until model.lines intersect (top until top + height)

  def cursor(p: Pos): Unit = {
    if (visibleLine(p.line)) {
      wmove(win, p.line - top, p.col)
    }
  }

  def close(): Unit = {
    model unsubscribe this
    delwin(win)
  }
}

class TextModel(init: String = null) {
  val text = new ArrayBuffer[ArrayBuffer[Char]]()

  if (init eq null)
    text += new ArrayBuffer[Char]
  else {
    for (l <- io.Source.fromString(init).getLines())
      text += (ArrayBuffer[Char]() ++ l)
  }

  val subscribers = new ArrayBuffer[TextView]

  var exptabs = false
  var tabs    = 2

  def subscribe(view: TextView): Unit = subscribers += view

  def unsubscribe(view: TextView): Unit = subscribers -= view

  def event(e: Event): Unit = subscribers foreach (_ react e)

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

  def start(p: Pos): Option[Pos] =
    if (p.col > 0) Some(p.copy(col = 0))
    else None

  def end(p: Pos): Option[Pos] = {
    val endp = char2col(p.line, text(p.line).length)

    if (p.col < endp.col) Some(endp)
    else None
  }

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

  def delete(p: Pos): Pos = {
    val char           = col2char(p)
    val Pos(line, col) = p

    if (char < text(line).length) {
      text(line).remove(char)
      event(LineChange(line, col, slice(line, char)))
    } else if (line < text.length - 1) {
      text(line).addAll(text(line + 1))
      text.remove(line + 1)
      event(DocumentChange(line))
    }

    p
  }

  def slice(line: Int, from: Int, until: Int): String = text(line).slice(from, until).mkString

  def slice(line: Int, from: Int): String = slice(line, from, text(line).length)

  def insert(p: Pos, c: Char): Pos = {
    val char           = col2char(p)
    val Pos(line, col) = p

    c match {
      case '\t' =>
        val spaces = tabs - col % tabs

        text(line).insertAll(char, if (exptabs) " " * spaces else "\t")
        event(LineChange(line, col, slice(line, char)))
        Pos(line, col + spaces)
      case '\n' =>
        text.insert(line + 1,
                    if (text(line).length > char) text(line).slice(char, text(line).length)
                    else new ArrayBuffer[Char])

        if (text(line).length > char) {
          text(line).remove(char, text(line).length - char)
          event(DocumentChange(line))
        } else
          event(DocumentChange(line + 1))

        Pos(line + 1, 0)
      case _ =>
        text(line).insert(char, c)
        event(LineChange(line, col, slice(line, char)))
        Pos(line, col + 1)
    }
  }

  def getLine(line: Int): String = text(line).mkString

}
