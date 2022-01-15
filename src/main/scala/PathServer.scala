import java.io.{BufferedReader, FileNotFoundException, InputStreamReader, PrintWriter}
import little.json.*
import little.json.Implicits.{*, given}
import com.nicolaswinsten.wikiracer.WikiRacer

import java.net.Socket
import scala.jdk.CollectionConverters.*
import concurrent.duration.DurationInt
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import java.util.Date

@main
def main =
  val port = if(System.getenv("PORT") != null) System.getenv("PORT").toInt else 8080
  println(s"running on port $port")
  val server = java.net.ServerSocket(port)

  while true do
    val socket = server.accept()
    handleConnection(socket)

end main

def httpResponse(json: JsonObject) =
  s"""HTTP/1.1 200 OK
     |Date: ${new Date}
     |Server: PathFinder
     |Content-Type: application/json
     |Connection: Closed
     |
     |${json}
     |\r\n""".stripMargin

def failed(msg: JsonValue) = Json.obj("failed" -> msg)

// connect up the titles using WikiRacer
def findPath(titles: Seq[String]) : Try[List[String]] =
  Try {
    println("attempting path")
    val racer = new WikiRacer()
    titles sliding 2 map {
      case Seq(source, dest) => racer.findWikiLadder(source, dest).asScala.toList
    } reduce {
      case (fullPath, nextSegment) => fullPath ++ nextSegment.tail
    }
  }

def timer = Future { Thread.sleep(3.minutes.toMillis); failed("timeout") }

def handleConnection(socket: Socket) =
  val in = BufferedReader(InputStreamReader(socket.getInputStream))
  val out = PrintWriter(socket.getOutputStream)

  val input = LazyList.continually(in.readLine())
    .filter(_ != null)
    .takeWhile(_.length != 0)

  val titles =
    input.find(_ startsWith "GET ") flatMap { getReq =>
      println("client says: " + getReq)
      getReq split " " apply 1 drop 1 split "/" filter (_.length > 0) to List match {
        case ts if ts.length >= 2 =>
          println(s"received $ts")
          Some(ts)
        case o =>
          None
      }
    } match {
      case Some(ts) => Success(ts)
      case None => Failure(new RuntimeException("malformed request"))
    }


  val attemptFindPath = Future {
    titles flatMap findPath match
      case Success(path) => Json.obj("path" -> path)
      case Failure(e) => failed(e.getMessage)
  }

  val jsonResponse = Future firstCompletedOf Seq(timer, attemptFindPath)

  jsonResponse onComplete { fTry =>
    val json = fTry match
      case Success(msg) => msg
      case Failure(e) => e.printStackTrace(); failed("Something went wrong")

    println(s"returning $json")

    out.write(httpResponse(json))
    out.flush()

    in.close()
    out.close()
    socket.close()
  }
end handleConnection