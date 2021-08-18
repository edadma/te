package xyz.hyperreal.te

import scopt.OParser
import xyz.hyperreal.ncurses.LibNCurses._

import java.io.File
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.scalanative.unsafe._

object Main extends App {
  case class Config(file: Option[File])

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
      arg[Option[File]]("<file>")
        .optional()
        .action((f, c) => c.copy(file = f))
        .validate(f =>
          if (!f.get.exists || f.get.isFile && f.get.canRead) success
          else failure("<file> must be a readable file if it exists"))
        .text("path to text file to open")
    )
  }

  OParser.parse(parser, args, Config(None)) match {
    case Some(Config(Some(file))) => app(file)
    case Some(Config(None))       => app(new File("untitled"))
    case _                        =>
  }

  def app(file: File): Unit = {
    initscr

    //  val init     = Files.readString(Paths.get("build.sbt"))
    val init = {
      if (file.exists) util.Using(io.Source.fromFile(file.getPath))(_.mkString).get
      else ""
    }
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
    val view     = new TextView(new TextModel(file.getPath, init), getmaxy(stdscr) - 3, getmaxx(stdscr), 2, 0)
    var pos: Pos = null

    def home(): Unit = cursor(Pos(0, 0))

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

//    @tailrec
//    def listen(): Unit = {
//      fromCString(keyname(wgetch(view.win))) match {
//        case "^C"            => return
//        case "KEY_HOME"      => view.model.startOfLine(pos) foreach cursor
//        case "KEY_END"       => view.model.endOfLine(pos) foreach cursor
//        case "kHOM5"         => home()
//        case "kEND5"         => cursor(view.model.end)
//        case "KEY_PPAGE"     => view.model.up(pos, view.height) foreach cursor
//        case "KEY_NPAGE"     => view.model.down(pos, view.height) foreach cursor
//        case "KEY_UP"        => view.model.up(pos, 1) foreach cursor
//        case "KEY_DOWN"      => view.model.down(pos, 1) foreach cursor
//        case "KEY_LEFT"      => view.model.left(pos) foreach cursor
//        case "KEY_RIGHT"     => view.model.right(pos) foreach cursor
//        case "KEY_BACKSPACE" => view.model.backspace(pos) foreach cursor
//        case "KEY_DC"        => view.cursor(view.model.delete(pos))
//        case "^J"            => cursor(view.model.insertBreak(pos))
//        case "^I"            => cursor(view.model.insertTab(pos))
//        case s               => cursor(view.model.insert(pos, s.head))
//      }
//
//      listen()
//    }

    raw
    noecho
    keypad(view.win, bf = true)
    scrollok(view.win, bf = true)
    nodelay(view.win, bf = true)

    Event.handler = {
      case DocumentChangeEvent(views, line) =>
        for (v <- views)
          v.render(v.visibleFrom(line))

        view.cursor(pos)
      case LineChangeEvent(views, line, from, chars) =>
        for (v <- views)
          if (v.visibleLine(line))
            v.render(line, from, chars)

        view.cursor(pos)
      case SegmentChangeEvent(views, line, from, count, chars) =>
      case KeyEvent("^C") =>
        endwin
        sys.exit()
      case KeyEvent("KEY_HOME")      => view.model.startOfLine(pos) foreach cursor
      case KeyEvent("KEY_END")       => view.model.endOfLine(pos) foreach cursor
      case KeyEvent("kHOM5")         => home()
      case KeyEvent("kEND5")         => cursor(view.model.end)
      case KeyEvent("KEY_PPAGE")     => view.model.up(pos, view.height) foreach cursor
      case KeyEvent("KEY_NPAGE")     => view.model.down(pos, view.height) foreach cursor
      case KeyEvent("KEY_UP")        => view.model.up(pos, 1) foreach cursor
      case KeyEvent("KEY_DOWN")      => view.model.down(pos, 1) foreach cursor
      case KeyEvent("KEY_LEFT")      => view.model.left(pos) foreach cursor
      case KeyEvent("KEY_RIGHT")     => view.model.right(pos) foreach cursor
      case KeyEvent("KEY_BACKSPACE") => view.model.backspace(pos) foreach cursor
      case KeyEvent("KEY_DC")        => view.cursor(view.model.delete(pos))
      case KeyEvent("^J")            => cursor(view.model.insertBreak(pos))
      case KeyEvent("^I")            => cursor(view.model.insertTab(pos))
      case KeyEvent(s)               => cursor(view.model.insert(pos, s.head))
    }

    Event phase {
      val k = wgetch(view.win)

      if (k != ERR)
        Event(KeyEvent(fromCString(keyname(k))))
    }

    home()
    Event.start()
  }

}

case class Pos(line: Int, col: Int)

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
