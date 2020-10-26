package zio.internal.debugging

object LayoutHelper {
  def colored(code: String)(str: String): String = s"$code$str${Console.RESET}"
  lazy val black: String => String               = colored(Console.BLACK)
  lazy val red: String => String                 = colored(Console.RED)
  lazy val green: String => String               = colored(Console.GREEN)
  lazy val yellow: String => String              = colored(Console.YELLOW)
  lazy val blue: String => String                = colored(Console.BLUE)
  lazy val magenta: String => String             = colored(Console.MAGENTA)
  lazy val cyan: String => String                = colored(Console.CYAN)
  lazy val white: String => String               = colored(Console.WHITE)

  def exactlyN(string: String, n: Int): String =
    if (!string.isEmpty) {
      val first   = string.lines().iterator().next()
      val trimmed = first.substring(0, Math.min(first.length(), n))
      val delta   = n - trimmed.length
      val padding = " " * delta
      trimmed + padding
    } else " " * n
}
