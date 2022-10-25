package io.github.edadma

import java.io.PrintWriter

package object te {

  lazy val logfile = new PrintWriter(s"log")

  def log(x: Any): Unit = {
    logfile.println(x)
    logfile.flush()
  }

}
