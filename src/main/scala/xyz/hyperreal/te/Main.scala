package xyz.hyperreal.te

import scopt.OParser
import xyz.hyperreal.ncurses.LibNCurses._

import java.io.File
import scala.scalanative.unsafe._

object Main extends App {
  case class Config(file: Option[File])

  val builder = OParser.builder[Config]

  val parser = {
    import builder._

    OParser.sequence(
      programName("te"),
      head("Terminal Editor", "v0.1.0"),
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
    val view                  = new TextView(new TextModel(file.getAbsolutePath, init), getmaxy(stdscr) - 3, getmaxx(stdscr), 2, 0)
    var pos: Pos              = null
    var notification: String  = ""
    var removalTimer: Timeout = null

    def home(): Unit = cursor(Pos(0, 0))

    def cursor(p: Pos): Unit = {
      pos = p
      status()
    }

    def notify(text: String): Unit = {
      notification = text
      status()
    }

    def status(): Unit = Zone { implicit z =>
      move(getmaxy(stdscr) - 1, 0)
      wbkgdset(stdscr, ' ' | A_REVERSE | A_DIM)

      val status = s"${pos.line + 1}:${pos.col + 1}  LF  UTF-8  2-spaces"

      clrtoeol
      move(getmaxy(stdscr) - 1, 0)
      addstr(toCString(notification))
      move(getmaxy(stdscr) - 1, getmaxx(stdscr) - status.length)
      addstr(toCString(status))
      wbkgdset(stdscr, ' ')
      refresh
      view.cursor(pos)
    }

//    Event.reactions += {
//      case e => log(e)
//    }

    Event.reactions += {
      case DocumentLoadEvent(views) =>
        views foreach (_.viewport(0))
        home()
      case LinesChangeEvent(views, line) =>
        for (v <- views)
          v.render(v.visibleFrom(line))

        view.cursor(pos)
      case LineChangeEvent(views, line, from, chars) =>
        for (v <- views)
          if (v.visibleLine(line))
            v.render(line, from, chars)

        view.cursor(pos)
      case SegmentChangeEvent(views, line, from, count, chars) =>
      case DocumentModifiedEvent(model)                        =>
      case KeyEvent("^C")                                      => Event.stop() //todo: remove this case
      case KeyEvent("KEY_HOME")                                => view.model.startOfLine(pos) foreach cursor
      case KeyEvent("KEY_END")                                 => view.model.endOfLine(pos) foreach cursor
      case KeyEvent("kHOM5")                                   => home()
      case KeyEvent("kEND5")                                   => cursor(view.model.end)
      case KeyEvent("KEY_PPAGE")                               => view.model.up(pos, view.height) foreach cursor
      case KeyEvent("KEY_NPAGE")                               => view.model.down(pos, view.height) foreach cursor
      case KeyEvent("KEY_UP")                                  => view.model.up(pos, 1) foreach cursor
      case KeyEvent("KEY_DOWN")                                => view.model.down(pos, 1) foreach cursor
      case KeyEvent("KEY_LEFT")                                => view.model.left(pos) foreach cursor
      case KeyEvent("KEY_RIGHT")                               => view.model.right(pos) foreach cursor
      case KeyEvent("KEY_BACKSPACE")                           => view.model.backspace(pos) foreach cursor
      case KeyEvent("KEY_DC")                                  => view.cursor(view.model.delete(pos))
      case KeyEvent("^J")                                      => cursor(view.model.insertBreak(pos))
      case KeyEvent("^I")                                      => cursor(view.model.insertTab(pos))
      case KeyEvent("^S")                                      => view.model.save()
      case KeyEvent(k) if k.startsWith("^") && k.length > 1    =>
      case KeyEvent(s)                                         => cursor(view.model.insert(pos, s.head))
      case NotificationEvent(text) =>
        if (removalTimer ne null)
          Event.cancel(removalTimer)

        notify(text)
        removalTimer = Event.timeout(5 * 1000) { notify("") }
    }

    Event phase {
      val k = wgetch(view.win)

      if (k != ERR)
        fromCString(keyname(k)) match {
          case "KEY_MOUSE" => Event(MouseEvent(""))
          case k           => Event(KeyEvent(k))
        }
    }

    raw
    noecho
    keypad(view.win, bf = true)
    scrollok(view.win, bf = true)
    nodelay(view.win, bf = true)
    Event(DocumentLoadEvent(Seq(view)))
    Event.start()
    endwin
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

//"/usr/bin/ld" -export-dynamic -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -export-dynamic -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o /home/ed/dev-sn/te/target/scala-2.13/te-out /usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu/crt1.o /usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/10/crtbegin.o -L//home/ed/dev-sn/te/native-lib -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/usr/lib/x86_64-linux-gnu/../../lib64 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../.. -L/usr/lib/llvm-12/bin/../lib -L/lib -L/usr/lib /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/utime.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/netdb.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/limits.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/termios.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/uio.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/stat.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/socket.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/uname.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/ioctl.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/socket_conversions.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/select.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/statvfs.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/unistd.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/dirent.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/syslog.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/grp.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/pwd.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/arpa/inet.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/pthread.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/time.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/fcntl.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/netinet/in.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/netinet/tcp.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/cpio.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/errno.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-ncurses_native0.4_2.13-1/scala-native/macros.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/process_monitor.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/UnwindRegistersSave.S.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/UnwindRegistersRestore.S.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/Unwind_AppleExtras.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/Unwind-EHABI.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/libunwind.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/UnwindLevel1-gcc-ext.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/UnwindLevel1.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/Unwind-sjlj.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/shutdown.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/platform.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/unwind.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/eh.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/LargeAllocator.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Marker.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/State.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Object.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/StackTrace.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Stats.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Allocator.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Heap.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/ImmixGC.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Settings.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/BlockAllocator.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/datastructures/BlockList.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/datastructures/Bytemap.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/datastructures/Stack.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Block.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/time_millis.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/time_nano.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/dyndispatch.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/math.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/stdlib.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/complex.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/stdio.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/float.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/signal.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/errno.c.o /home/ed/dev-sn/te/target/scala-2.13/native/4.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/8.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/11.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/3.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/7.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/2.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/9.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/6.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/1.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/10.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/5.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/0.ll.o -lpthread -ldl -lstdc++ -lm -lgcc_s -lgcc -lc -lgcc_s -lgcc /usr/bin/../lib/gcc/x86_64-linux-gnu/10/crtend.o /usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu/crtn.o

//"/usr/bin/ld" -export-dynamic -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -export-dynamic -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o /home/ed/dev-sn/te/target/scala-2.13/te-out /usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu/crt1.o /usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/10/crtbegin.o -L//home/ed/dev-sn/te/native-lib -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../../lib64 -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib64 -L/usr/lib/x86_64-linux-gnu/../../lib64 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../.. -L/usr/lib/llvm-12/bin/../lib -L/lib -L/usr/lib /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/utime.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/netdb.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/limits.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/termios.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/uio.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/stat.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/socket.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/uname.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/ioctl.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/socket_conversions.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/sys/select.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/statvfs.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/unistd.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/dirent.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/syslog.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/grp.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/pwd.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/arpa/inet.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/pthread.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/time.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/fcntl.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/netinet/in.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/netinet/tcp.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/cpio.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-posixlib_native0.4_2.13-0.4.0-2/scala-native/errno.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-ncurses_native0.4_2.13-1/scala-native/macros.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/process_monitor.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/UnwindRegistersSave.S.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/UnwindRegistersRestore.S.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/Unwind_AppleExtras.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/Unwind-EHABI.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/libunwind.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/UnwindLevel1-gcc-ext.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/UnwindLevel1.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/libunwind/Unwind-sjlj.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/shutdown.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/platform.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/unwind.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/eh.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/LargeAllocator.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Marker.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/State.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Object.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/StackTrace.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Stats.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Allocator.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Heap.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/ImmixGC.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Settings.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/BlockAllocator.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/datastructures/BlockList.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/datastructures/Bytemap.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/datastructures/Stack.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/gc/immix/Block.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/time_millis.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/time_nano.cpp.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-nativelib_native0.4_2.13-0.4.0-0/scala-native/dyndispatch.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/math.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/stdlib.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/complex.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/stdio.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/float.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/signal.c.o /home/ed/dev-sn/te/target/scala-2.13/native/native-code-clib_native0.4_2.13-0.4.0-3/scala-native/errno.c.o /home/ed/dev-sn/te/target/scala-2.13/native/4.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/8.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/11.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/3.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/7.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/2.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/9.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/6.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/1.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/10.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/5.ll.o /home/ed/dev-sn/te/target/scala-2.13/native/0.ll.o -lpthread -ldl -lncurses -lstdc++ -lm -lgcc_s -lgcc -lc -lgcc_s -lgcc /usr/bin/../lib/gcc/x86_64-linux-gnu/10/crtend.o /usr/bin/../lib/gcc/x86_64-linux-gnu/10/../../../x86_64-linux-gnu/crtn.o
