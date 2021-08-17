package xyz.hyperreal.te

import xyz.hyperreal.ncurses.LibNCurses._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.scalanative.unsafe.{Zone, toCString}

object Main extends App {

  initscr
  cbreak
  noecho
  keypad(stdscr, bf = true)

  val view      = new TextView(new TextModel, getmaxy(stdscr) - 3, getmaxx(stdscr), 2, 0)
  var line: Int = 0
  var col: Int  = 0

  view.cursor(line, col)

  Zone { implicit z =>
    @tailrec
    def edit(): Unit = {
      val c = getch

      if (c == KEY_LEFT)
        buf.left
      else if (c == KEY_RIGHT)
        buf.right
      else {
        val line = buf.line

        if (c == KEY_BACKSPACE)
          buf.backspace
        else if (c == KEY_DC)
          buf.delete
        else
          buf.insert(c.toChar)

        if (line != buf.line) {
          move(line + 2, 0)
          clrtobot

          val rows = getmaxy(stdscr)

          //print(buf.text)

          for (i <- (line min buf.line) until (rows min buf.lines)) {
            move(i + 2, 0)
            addstr(toCString(buf.getLine(i)))
          }
        } else {
          move(buf.line + 2, 0)
          Zone(implicit z => addstr(toCString(buf.getCurrentLine)))
          clrtoeol
        }
      }

      move(buf.line + 2, buf.col)
      edit()
    }

    edit()
  }

  endwin

}

trait Event
case class LineChange(line: Int, from: Int) extends Event
case class LinesChange(line: Int)           extends Event

class TextView(val model: TextModel, nlines: Int, ncols: Int, begin_y: Int, begin_x: Int) {
  val win: WINDOW = newwin(nlines, ncols, begin_y, begin_x)

  model subscribe this

  var tline: Int = 0
  var lines: Int = 0

  def cursor(line: Int, col: Int): Unit = {
    wmove(win, line, col)
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

  def lines: Int = text.length

  def char2col(line: Int, char: Int): Int = {
    var c = 0
    val s = text(line)

    for (i <- 0 until char)
      c += (if (s(i) == '\t') tabs - c % tabs else 1)

    c
  }

  def left: Boolean =
    if (cchar > 0) {
      cchar -= 1
      true
    } else if (cline > 0) {
      cline -= 1
      cchar = text(cline).length
      true
    } else false

  def right: Boolean =
    if (cchar < text(cline).length) {
      cchar += 1
      true
    } else if (cline < text.length - 1) {
      cline += 1
      cchar = 0
      true
    } else false

  def backspace: Boolean = {
    if (left) {
      delete
      true
    } else false
  }

  def delete: Boolean =
    if (cchar < text(cline).length) {
      text(cline).remove(cchar)
      true
    } else if (cline < text.length - 1) {
      text(cline).addAll(text(cline + 1))
      text.remove(cline + 1)
      true
    } else false

  def insert(c: Char): Unit =
    c match {
      case '\t' if exptabs =>
        val spaces = tabs - c % tabs

        text(cline).insertAll(cchar, " " * spaces)
        cchar += spaces
      case '\n' => insertBreak()
      case _ =>
        text(cline).insert(cchar, c)
        cchar += 1
    }

  def insertBreak(): Unit = {
    text.insert(cline + 1,
                if (text(cline).length > cchar) text(cline).slice(cchar, text(cline).length)
                else new ArrayBuffer[Char])

    if (text(cline).length > cchar)
      text(cline).remove(cchar, text(cline).length - cchar)

    cline += 1
    cchar = 0
  }

  def getLine(l: Int): String = text(l).mkString

  def getCurrentLine: String = getLine(cline)

  //def getToEndOfLine: String =

}
