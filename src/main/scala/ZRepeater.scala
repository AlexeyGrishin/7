import java.io.{BufferedReader, InputStreamReader}
import java.net.{HttpURLConnection, URL}

import scala.sys.process._

//вспомогательная утилита - вызывает стандартный repeat и последнюю версию стратегии - чтобы получить лог
object ZRepeater extends App {

  val cookie = """<>"""

  if (args.length == 0) {
    println("no url")
  }
  else {
    var url = args(0)

    def tokenByUrl(url: String): Option[String] = {
      if (!url.startsWith("http")) return Some(url)
      val connection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestProperty("Cookie", cookie)
      val br = new BufferedReader(new InputStreamReader(connection.getInputStream, "UTF-8"))
      var line: java.lang.String = br.readLine()
      while (line != null) {
        if (line.matches("^\\s+[a-z0-9]{40}\\s*$")) {
          return Option(line.trim)
        }
        line = br.readLine()
      }
      None
    }

    tokenByUrl(url) match {
      case Some(token) => {
        println(s"token is $token")
        val p = s"java -jar repeater/repeater.jar $token".run()
        Runner.main(Array(if (args.length == 1) token else args(1)))
        p.destroy()
        System.exit(0)
      }
      case None =>
        println("no token :( maybe auth cookie is out of date")
    }
  }
}
