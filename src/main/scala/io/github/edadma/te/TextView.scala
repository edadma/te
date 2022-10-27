package io.github.edadma.te

import io.github.edadma.ncurses._

class TextView(val model: TextModel, nlines: Int, val ncols: Int, begin_y: Int, begin_x: Int) {
  val win: Window = newwin(nlines, ncols, begin_y, begin_x)
  val panel: Panel = new_panel(win)

  model subscribe this

  var top: Int = _

  def resize(lines: Int, cols: Int): Unit = {
    win.wresize(lines, cols)
    render()
  }

  def render(): Unit = {
    renderToBottom(top until ((top + height) min model.lines))
  }

  def renderToBottom(range: Seq[Int]): Unit = {
    render(range)
    win.clrtobot
  }

  def viewport(from: Int): Unit = {
    top = from
    render()
  }

  def render(line: Int, from: Int, chars: String): Unit = {
    win.addstr(line - top, from, chars)
    win.clrtoeol
  }

  def render(range: Seq[Int]): Unit =
    for (i <- range)
      render(i, 0, model.getLine(i))

  def width: Int = win.getmaxx

  def height: Int = win.getmaxy

  def visibleLine(line: Int): Boolean = top <= line && line < top + height

  def visibleFrom(line: Int): Seq[Int] = line until model.lines intersect (top until top + height)

  def cursor(p: Pos): Unit = {
    if (!visibleLine(p.line)) {
      if (p.line < top && top - p.line < height) {
        val n = top - p.line

        win.scrl(-n)

        val oldtop = top

        top = p.line
        render(p.line until oldtop)
      } else if (p.line >= top + height && p.line - (top + height) < height) {
        val n = p.line - (top + height) + 1

        win.scrl(n)
        top += n
        render(top + height - n until top + height)
      } else
        viewport(p.line)
    }

    win.move(p.line - top, p.col)
  }

  def close(): Unit = {
    model unsubscribe this
    win.delwin
  }
}
