package xyz.hyperreal.te

import xyz.hyperreal.ncurses.{LibNCurses => nc, LibNCursesHelpers => nch}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.scalanative.unsafe.{Zone, toCString}

object Main extends App {

  nc.initscr
  nc.cbreak
  nc.noecho
  nc.keypad(nc.stdscr, bf = true)

  val buf = new TextBuffer

  nc.move(2, 0)

  @tailrec
  def edit(): Unit = {
    val c    = nc.getch
    val line = buf.line

    if (c == nc.KEY_LEFT)
      buf.left
    else if (c == nc.KEY_RIGHT)
      buf.right
    else if (c == nc.KEY_BACKSPACE)
      buf.backspace
    else
      buf.insert(c.toChar)

    if (line != buf.line) {
      nc.move(line + 2, 0)
      Zone(implicit z => nc.addstr(toCString(buf.getLine(line))))
      nc.clrtoeol
    }

    nc.move(buf.line + 2, 0)
    Zone(implicit z => nc.addstr(toCString(buf.getCurrentLine)))
    nc.clrtoeol
    nc.move(buf.line + 2, buf.col)
    edit()
  }

  edit()

  nc.endwin

}

class TextBuffer {
  val lines = new ArrayBuffer[ArrayBuffer[Char]]()

  lines += new ArrayBuffer[Char]

  var exptabs = true
  var tabs    = 2
  var cline   = 0
  var cchar   = 0

  def line: Int = cline

  def col: Int = {
    var c = 0
    val s = lines(cline)

    for (i <- 0 until cchar)
      c += (if (s(i) == '\t') tabs - c % tabs else 1)

    c
  }

  def left: Boolean =
    if (cchar > 0) {
      cchar -= 1
      true
    } else if (cline > 0) {
      cline -= 1
      cchar = lines(cline).length
      true
    } else false

  def right: Boolean =
    if (cchar < lines(cline).length) {
      cchar += 1
      true
    } else if (cline < lines.length - 1) {
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
    if (cchar < lines(cline).length) {
      lines(cline).remove(cchar)
      true
    } else if (cline < lines.length - 1) {
      lines.remove(cline)
      true
    } else false

  def insert(c: Char): Unit =
    c match {
      case '\t' if exptabs =>
        val spaces = tabs - c % tabs

        lines(cline).insertAll(cchar, " " * spaces)
        cchar += spaces
      case '\n' => insertBreak()
      case _ =>
        lines(cline).insert(cchar, c)
        cchar += 1
    }

  def insertBreak(): Unit = {
    lines.insert(cline + 1,
                 if (lines(cline).length > cchar) lines(cline).slice(cchar, lines(cline).length)
                 else new ArrayBuffer[Char])

    if (lines(cline).length > cchar)
      lines(cline).remove(cchar, lines(cline).length - cchar)

    cline += 1
    cchar = 0
  }

  def getLine(l: Int): String = lines(l).mkString

  def getCurrentLine: String = getLine(cline)

  //def getToEndOfLine: String =

}
