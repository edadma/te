package xyz.hyperreal

import xyz.hyperreal.ncurses.LibNCurses._

import scala.scalanative.unsafe._

package object te {

  def message(x: Any): CInt = {
    move(getmaxy(stdscr) - 1, 0)
    attron(A_REVERSE | A_DIM)
    Zone(implicit z => addstr(toCString("%-20s".format(String.valueOf(x)))))
    attroff(A_REVERSE)
    refresh
  }

}
