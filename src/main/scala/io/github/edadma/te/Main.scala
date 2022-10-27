package io.github.edadma.te

import scopt.OParser
import io.github.edadma.ncurses._

import java.io.File
import scala.collection.mutable.ArrayBuffer

object Main extends App {
  case class Config(file: File, encoding: String)

  val builder = OParser.builder[Config]
  val parser = {
    import builder._

    OParser.sequence(
      programName("te"),
      head("Terminal Editor", "v0.1.0.alpha"),
      help('h', "help").text("prints this usage text"),
      opt[String]('e', "encoding")
        .optional()
        .action((e, c) => c.copy(encoding = e))
        .validate(e =>
          if (e == "UTF-8") success
          else failure(s"invalid character encoding scheme: $e"),
        )
        .text("set character encoding scheme"),
      version('v', "version").text("prints the version"),
      arg[File]("<file>")
        .optional()
        .action((f, c) => c.copy(file = f))
        .validate(f =>
          if (!f.exists || f.isFile && f.canRead) success
          else failure("<file> must be a readable file if it exists"),
        )
        .text("path to text file to open"),
    )
  }

  OParser.parse(parser, args, Config(new File("untitled"), "UTF-8")) match {
    case Some(conf) => app(conf)
    case _          =>
  }

  def app(conf: Config): Unit = {
    initscr

    val init =
      if (conf.file.exists) util.Using(scala.io.Source.fromFile(conf.file.getPath, conf.encoding))(_.mkString).get
      else ""
    val view = new TextView(new TextModel(conf.file.getAbsolutePath, init), stdscr.getmaxy - 3, stdscr.getmaxx, 2, 0)
    try {
      val buffers = new ArrayBuffer[TextView] :+ view
      var pos: Pos = null
      var notification: String = ""
      var removalTimer: Timeout = null

      def home(): Unit = cursor(Pos(0, 0))

      def tabs(): Unit = {
        move(1, 0)

        bkgdset(' ' | A_REVERSE | A_DIM)
        clrtoeol
        move(1, 0)

        for (b <- buffers) {
          addstr(s" ${new File(b.model.path).getName} ${if (b.model.unsaved) '*' else ' '} ")
          addch(ACS_VLINE)
        }

        refresh
        view.cursor(pos)
      }

      def cursor(p: Pos): Unit = {
        pos = p
        status()
      }

      def notify(text: String): Unit = {
        notification = text
        status()
      }

      def status(): Unit = {
        move(stdscr.getmaxy - 1, 0)
        bkgdset(' ' | A_REVERSE | A_DIM)

        val status = s"${pos.line + 1}:${pos.col + 1}  LF  UTF-8  2 spaces  exp"

        clrtoeol
        addstr(stdscr.getmaxy - 1, 0, notification)
        addstr(stdscr.getmaxy - 1, stdscr.getmaxx - status.length, status)
        bkgdset(' ')
        refresh
        view.cursor(pos)
      }

      Event.reactions += {
        case DocumentLoadEvent(views) =>
          views foreach (_.viewport(0))
          home()
          tabs()
        case DocumentSaveEvent(model) =>
          tabs()

          if (removalTimer ne null)
            Event.cancel(removalTimer)

          notify(s""""${model.path}" saved""")
          removalTimer = Event.timeout(5 * 1000) {
            notify("")
          }
        case LinesChangeEvent(views, line) =>
          for (v <- views)
            v.renderToBottom(v.visibleFrom(line))

          clrtobot
          view.cursor(pos)
        case LineChangeEvent(views, line, from, chars) =>
          for (v <- views)
            if (v.visibleLine(line))
              v.render(line, from, chars)

          view.cursor(pos)
        case SegmentChangeEvent(views, line, from, count, chars) =>
        case DocumentModifiedEvent(model)                        => tabs()
        case KeyEvent("^Q") =>
          for (b <- buffers)
            if (b.model.unsaved)
              b.model.save()

          Event.stop()
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
        case KeyEvent("KEY_DC")        => view.cursor(view.model.delete(pos, 1))
        case KeyEvent("kLFT5")         => view.model.leftWord(pos) foreach cursor
        case KeyEvent("kRIT5")         => view.model.rightWord(pos) foreach cursor
        case KeyEvent("^H")            => view.model.backspaceWord(pos) foreach cursor
        case KeyEvent("kDC5")          => view.model.deleteWord(pos) foreach cursor
        case KeyEvent("^J")            => cursor(view.model.insertBreak(pos))
        case KeyEvent("^I")            => cursor(view.model.insertTab(pos))
        case KeyEvent("^S")            => view.model.save()
        case KeyEvent("^Z") =>
          view.model.afterLast match {
            case Some(after) => cursor(if (after != pos) after else view.model.undo)
            case None        =>
          }
        case KeyEvent(k) if k.startsWith("^") && k.length > 1 =>
        case KeyEvent(s)                                      => cursor(view.model.insert(pos, s))
        case ResizeEvent =>
          view.resize(stdscr.getmaxy - 3, stdscr.getmaxx)
          tabs()

          val p = pos copy (line = pos.line min (view.top + view.height - 1))

          cursor(p copy (col = p.col min (view.width - 1) min (view.model.getLine(p.line).length - 1)))
      }

      Event phase {
        val k = view.win.getch

        if (k != ERR)
          keyname(k) match {
            case "KEY_MOUSE"  => Event(MouseEvent(""))
            case "KEY_RESIZE" => Event(ResizeEvent)
            case k            => Event(KeyEvent(k))
          }
      }

      raw
      noecho
      view.win.keypad(bf = true)
      view.win.scrollok(bf = true)
      view.win.nodelay(bf = true)
      Event(DocumentLoadEvent(Seq(view)))
      Event.start()
      endwin
    } catch {
      case e: Throwable =>
        endwin
        view.model.path ++= ".bak"
        view.model.save()
        println(
          s"Something bad and unexpected happened. An attempt was made to save a backup copy of your document at '${view.model.path}'.",
        )
        e.printStackTrace()
        sys.exit(1)
    }

  }
}

case class Pos(line: Int, col: Int) extends Ordered[Pos] {
  def compare(that: Pos): Int =
    line compare that.line match {
      case 0 => col compare that.col
      case c => c
    }
}

/*
todo: bug: ctrl-del at the beginning of a line with only a single space character on it and text above and below it
java.lang.RuntimeException: chars: different by more than lf
	at java.lang.Throwable.fillInStackTrace(Unknown Source)
	at scala.sys.package$.error(Unknown Source)
	at xyz.hyperreal.te.TextModel.chars(Unknown Source)
	at xyz.hyperreal.te.TextModel.$anonfun$deleteWord$1(Unknown Source)
	at xyz.hyperreal.te.TextModel$$Lambda$14.apply(Unknown Source)
	at scala.Option.map(Unknown Source)
	at xyz.hyperreal.te.TextModel.deleteWord(Unknown Source)
	at xyz.hyperreal.te.Main$$anonfun$app$4.applyOrElse(Unknown Source)
	at xyz.hyperreal.te.Main$$anonfun$app$4.applyOrElse(Unknown Source)
	at scala.runtime.AbstractPartialFunction.apply(Unknown Source)
	at xyz.hyperreal.te.Event$.$anonfun$start$4(Unknown Source)
	at xyz.hyperreal.te.Event$$$Lambda$4.apply(Unknown Source)
	at scala.collection.IterableOnceOps.foreach(Unknown Source)
	at scala.collection.AbstractIterable.foreach(Unknown Source)
	at xyz.hyperreal.te.Event$.$anonfun$start$3(Unknown Source)
	at xyz.hyperreal.te.Event$$$Lambda$3.apply(Unknown Source)
	at scala.Option.foreach(Unknown Source)
	at xyz.hyperreal.te.Event$.start(Unknown Source)
	at xyz.hyperreal.te.Main$.app(Unknown Source)
	at xyz.hyperreal.te.Main$.delayedEndpoint$xyz$hyperreal$te$Main$1(Unknown Source)
	at xyz.hyperreal.te.Main$delayedInit$body.apply(Unknown Source)
	at scala.Function0.apply$mcV$sp(Unknown Source)
	at scala.runtime.AbstractFunction0.apply$mcV$sp(Unknown Source)
	at scala.App.$anonfun$main$1(Unknown Source)
	at scala.App$$Lambda$1.apply(Unknown Source)
	at scala.collection.IterableOnceOps.foreach(Unknown Source)
	at scala.collection.AbstractIterable.foreach(Unknown Source)
	at scala.App.main(Unknown Source)
	at xyz.hyperreal.te.Main$.main(Unknown Source)
	at <none>.main(Unknown Source)
	at <none>.__libc_start_main(Unknown Source)
	at <none>._start(Unknown Source)
 */
