package xyz.hyperreal.te

import scopt.OParser
import xyz.hyperreal.ncurses.LibNCurses._
import xyz.hyperreal.ncurses.LibNCursesHelpers._

import java.io.File
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.scalanative.unsafe.{CInt, Zone, toCString}

object Main extends App {
  case class Config(file: File)

  val builder = OParser.builder[Config]

  val parser = {
    import builder._

    OParser.sequence(
      programName("te"),
      head("te", "v0.1.0"),
      help('h', "help").text("prints this usage text"),
//      opt[Unit]('v', "verbose")
//        .action((_, c) => c.copy(verbose = true))
//        .text("print internal actions"),
      version('v', "version").text("prints the version"),
      arg[File]("<file>")
        .action((f, c) => c.copy(file = f))
        .validate(f =>
          if (f.exists && f.isFile && f.canRead) success
          else failure("<file> must be a regular file that is readable"))
        .text("path to text file to open")
    )
  }

  OParser.parse(parser, args, Config(null)) match {
    case Some(config) => app(config.file)
    case _            =>
  }

  def app(file: File): Unit = {
    initscr

    //  val init     = Files.readString(Paths.get("build.sbt"))
    val init = util.Using(io.Source.fromFile(file.getPath))(_.mkString).get
//    val init =
//      """
//      | 1
//      | 2
//      | 3
//      | 4
//      | 5
//      | 6
//      | 7
//      | 8
//      | 9
//      |10
//      |11
//      |12
//      |13
//      |14
//      |15
//      |16
//      |17
//      |18
//      |19
//      |20
//      |21
//      |22
//      |23
//      |24
//      |25
//      |""".trim.stripMargin
    val view     = new TextView(new TextModel(init), getmaxy(stdscr) - 3, getmaxx(stdscr), 2, 0)
    val HOME     = Pos(0, 0)
    var pos: Pos = null

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

    cbreak
    noecho
    keypad(view.win, bf = true)
    scrollok(view.win, bf = true)
    home()
    listen()
    endwin
  }

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
    if (!visibleLine(p.line)) {
      if (p.line < top && top - p.line < height) {
        val n = top - p.line

//        bottom(s"scroll up: $n, height: $height")
        wscrl(win, -n)

        val oldtop = top

        top = p.line
        render(p.line until oldtop)
      } else if (p.line >= top + height && p.line - (top + height) < height) {
        val n = p.line - (top + height) + 1

//        bottom(s"scroll down: $n, height: $height")
        wscrl(win, n)
        top += n
        render(p.line until p.line + n)
      } else {
        bottom("viewport")
        viewport(p.line)
      }
    }

    wmove(win, p.line - top, p.col)
  }

  def bottom(x: Any): CInt = {
    move(getmaxy(stdscr) - 1, 0)
    Zone(implicit z => addstr(toCString(String.valueOf(x))))
    refresh
  }

  def close(): Unit = {
    model unsubscribe this
    delwin(win)
  }
}

class TextModel(init: String = null) {
  val text = new ArrayBuffer[ArrayBuffer[Char]]()

  if (init == null)
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

/*
TODO: handle resizing in a nice way
X window
If an xterm is resized the contents on your text windows might be messed up. To handle this gracefully you should redraw all the stuff based on the new height and width of the screen. When resizing happens, your program is sent a SIGWINCH signal. You should catch this signal and do redrawing accordingly. Here is some hint.


     #include <signal.h>
     void* resizeHandler(int);

     int main(void) {
          ...
          signal(SIGWINCH, resizeHandler);
          ...
     }

     void* resizeHandler(int sig)
     {
          int nh, nw;
          getmaxyx(stdscr, nh, nw);  /* get the new screen size */
          ...
     }
 */
