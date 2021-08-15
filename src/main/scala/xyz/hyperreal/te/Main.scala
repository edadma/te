package xyz.hyperreal.te

import xyz.hyperreal.ncurses.{LibNCurses => nc, LibNCursesHelpers => nch}

import scala.collection.mutable.ArrayBuffer

object Main extends App {

  nc.initscr
  nc.cbreak
  nc.noecho

  val buf = new TextBuffer

  nc.move(1, 0)

  def edit(): Unit = {
    nc.getch match {
      case ch => buf.insert(ch.toChar)
    }
  }

  edit()

  nc.endwin

}

class TextBuffer {
  val lines   = new ArrayBuffer[ArrayBuffer[Char]]
  var exptabs = true
  var tabs    = 2
  var cline   = 0
  var cchar   = 0

  def pos: (Int, Int) = {
    var c = 0
    val s = lines(cline)

    for (i <- 0 until cchar)
      c += (if (s(i) == '\t') tabs - c % tabs else 1)

    (cline + 1, c + 1)
  }

  def pos(rc: (Int, Int)): Unit = {}

  def backspace: Boolean = {
    false
  }

  def insert(c: Char): Unit =
    c match {
      case '\t' if exptabs =>
        val spaces = tabs - c % tabs

        lines(cline).insertAll(cchar, " " * spaces)
        cchar += spaces
      case '\n' => insertBreak()
      case _ =>
        lines(cline).insert(cchar) = c
        cchar += 1
    }

  def insertBreak(): Unit = {
    lines(cline) += (if (lines(cline).length > cchar) lines(cline).slice(cchar, cchar + lines(cline).length)
                     else Seq[Char]())
    cline += 1
  }

  def getLine: String = lines(cline).toString

  //def getToEndOfLine: String =

}
