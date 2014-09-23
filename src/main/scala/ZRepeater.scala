import java.io.{BufferedReader, InputStreamReader}
import java.net.{HttpURLConnection, URL}

import scala.sys.process._

object ZRepeater extends App {

  val cookie = """__utma=36177443.539816020.1385134977.1411350124.1411371899.37; __utmz=36177443.1410808852.26.2.utmcsr=vk.com|utmccn=(referral)|utmcmd=referral|utmcct=/feed; SMFCookie900=a%3A4%3A%7Bi%3A0%3Bs%3A4%3A%222661%22%3Bi%3A1%3Bs%3A40%3A%22c47509715ca04d32dfab41735f6f3cc09f3e337f%22%3Bi%3A2%3Bi%3A1442876581%3Bi%3A3%3Bi%3A0%3B%7D; JSESSIONID=90C12D35B4650F63D2E8E6F7FCE5A091; PHPSESSID=trton2i8mgqd2bo4qvqf2j2ja5; __utmb=36177443.20.10.1411371899; __utmc=36177443"""

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
        Runner.main(Array(token))
        p.destroy()
        System.exit(0)
      }
      case None =>
        println("no token :( maybe auth cookie is out of date")
    }
  }
}
