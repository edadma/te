package xyz.hyperreal.te

import xyz.hyperreal.ncurses.LibNCurses._

import scala.scalanative.unsafe._

class TextView(val model: TextModel, nlines: Int, val ncols: Int, begin_y: Int, begin_x: Int) {
  val win: WINDOW = newwin(nlines, ncols, begin_y, begin_x)

  model subscribe this

  var top: Int = _

  def resize(lines: Int, cols: Int): Unit = {
    wresize(win, lines, cols)
    render()
  }

  def render(): Unit = {
    render(top until ((top + height) min model.lines))
    wclrtobot(win)
  }

  def viewport(from: Int): Unit = {
    top = from
    render()
  }

  def render(line: Int, from: Int, chars: String): Unit = Zone { implicit z =>
    wmove(win, line - top, from)
    waddstr(win, toCString(chars))
    wclrtoeol(win)
  }

  def render(range: Seq[Int]): Unit =
    for (i <- range)
      render(i, 0, model.getLine(i))

  def width: Int = getmaxx(win)

  def height: Int = getmaxy(win)

  def visibleLine(line: Int): Boolean = top <= line && line < top + height

  def visibleFrom(line: Int): Seq[Int] = line until model.lines intersect (top until top + height)

  def cursor(p: Pos): Unit = {
    if (!visibleLine(p.line)) {
      if (p.line < top && top - p.line < height) {
        val n = top - p.line

        wscrl(win, -n)

        val oldtop = top

        top = p.line
        render(p.line until oldtop)
      } else if (p.line >= top + height && p.line - (top + height) < height) {
        val n = p.line - (top + height) + 1

        wscrl(win, n)
        top += n
        render(top + height - n until top + height)
      } else
        viewport(p.line)
    }

    wmove(win, p.line - top, p.col)
  }

  def close(): Unit = {
    model unsubscribe this
    delwin(win)
  }
}
