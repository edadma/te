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

  val HOME     = Pos(0, 0)
  val view     = new TextView(new TextModel, getmaxy(stdscr) - 3, getmaxx(stdscr), 2, 0)
  var pos: Pos = _

  def home(): Unit = cursor(HOME)

  def cursor(p: Pos): Unit = {
    pos = p
    move(getmaxy(stdscr) - 1, 0)
    printw("%d:%d", p.line - view.top + 1, p.col + 1)
    clrtoeol
    refresh
    view.cursor(p)
  }

  @tailrec
  def listen(): Unit = {
    val c = wgetch(view.win)

    if (c == KEY_HOME)
      home()
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

class TextView(val model: TextModel, nlines: Int, ncols: Int, begin_y: Int, begin_x: Int) {
  val win: WINDOW = newwin(nlines, ncols, begin_y, begin_x)

  model subscribe this

  var top: Int   = 0
  var lines: Int = 1

  viewport(top)

  def react(e: Event): Unit = Zone { implicit z =>
    e match {
      case DocumentChange(line) =>
        if (visible(line)) {
          val end = model.lines min (top + nlines)

          for (i <- line until end) {
            wmove(win, i - top, 0)
            waddstr(win, toCString(model.getLine(i)))
            wclrtoeol(win)
          }

          lines = end - line
          wclrtobot(win)
        }
      case LineChange(line, from, chars) =>
        if (visible(line)) {
          wmove(win, line - top, from)
          waddstr(win, toCString(chars))
          wclrtoeol(win)
        }
      case SegmentChange(line, from, count, chars) =>
    }
  }

  def viewport(line: Int): Unit = Zone { implicit z =>
    top = line

    val rows = getmaxy(win)

    for (i <- line until (rows min model.lines)) {
      wmove(win, i - top, 0)
      waddstr(win, toCString(model.getLine(i)))
    }
  }

  def visible(line: Int): Boolean = top <= line && line < top + nlines

  def cursor(p: Pos): Unit = {
    if (visible(p.line)) {
      wmove(win, p.line - top, p.col)
    }
  }

  def close(): Unit = {
    model unsubscribe this
    delwin(win)
  }
}

class TextModel {
  val text = new ArrayBuffer[ArrayBuffer[Char]]()

  text += new ArrayBuffer[Char]

  val subscribers = new ArrayBuffer[TextView]

  var exptabs = true
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

    while (cur < p.col) {
      cur += (if (s(char) == '\t') tabs - cur % tabs else 1)
      char += 1
    }

    char
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
      case '\t' if exptabs =>
        val spaces = tabs - col % tabs

        text(line).insertAll(char, " " * spaces)
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
