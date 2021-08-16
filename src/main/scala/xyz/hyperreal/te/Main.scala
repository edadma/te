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

  val buf = new TextModel

  nc.move(2, 0)

  Zone { implicit z =>
    @tailrec
    def edit(): Unit = {
      val c = nc.getch

      if (c == nc.KEY_LEFT)
        buf.left
      else if (c == nc.KEY_RIGHT)
        buf.right
      else {
        val line = buf.line

        if (c == nc.KEY_BACKSPACE)
          buf.backspace
        else
          buf.insert(c.toChar)

        if (line != buf.line) {
          nc.move(line + 2, 0)
          nc.clrtobot

          val rows = nc.getmaxy(nc.stdscr)

          //print(buf.text)

          for (i <- (line min buf.line) until (rows min buf.lines)) {
            nc.move(i + 2, 0)
            nc.addstr(toCString(buf.getLine(i)))
          }
        } else {
          nc.move(buf.line + 2, 0)
          Zone(implicit z => nc.addstr(toCString(buf.getCurrentLine)))
          nc.clrtoeol
        }
      }

      nc.move(buf.line + 2, buf.col)
      edit()
    }

    edit()
  }

  nc.endwin

}

class TextModel {
  val text = new ArrayBuffer[ArrayBuffer[Char]]()

  text += new ArrayBuffer[Char]

  var exptabs = true
  var tabs    = 2
  var cline   = 0
  var cchar   = 0

  def lines: Int = text.length

  def line: Int = cline

  def col: Int = {
    var c = 0
    val s = text(cline)

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
