package c.y.z
package utils

object Logger {
  var logLevel = 1
  def debug(msg: Any): Unit = {
    if (logLevel <= 1) println(msg)
  }
}
