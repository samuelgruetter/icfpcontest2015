import java.io.{ByteArrayOutputStream, PrintStream}

object Util {

  def printToString(printer: PrintStream => Unit): String = {
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)

    printer(ps)

    baos.toString("ISO-8859-1")
  }

}
